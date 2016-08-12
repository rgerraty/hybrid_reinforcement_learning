function [runs]=make_EV(dels)

%dels is seconds of deleted fmri volumes from beginning of scan
%note that right now you need to be in folder containing subjects .mat files. may want to change this

addpath /home/rgerraty/scripts/MATLAB/

[M,N]=csvreadh('/data/engine/rgerraty/hybrid_mri/behavior/hybrid_data.csv',',');


[status,runs]=unix('ls Performance_?.mat | cut -c13');

%how many runs
runs=str2num(runs);

[status,sub]=unix('pwd | cut -c43-44');
sub=str2num(sub);

%only need last run .mat file which contains all performance ingo
%try loading but quit if any discrepencies
clear Performance PerformanceMem
try
	load(strcat('Performance_',num2str(runs(end)),'.mat'))
catch
	warning(strcat('Performance file does not exist for run   ', num2str(runs), '. Exiting'))
	pwd
	return
end



valid_trials = ~isnan(Performance.choose.resp);

try
	load Performance_Memory
	submem=repmat(nan,size(Performance.time.startChoice));
	%HITS
	submem(sort(PerformanceMem.Cond.EncTrial(PerformanceMem.Resp.ObjRec<3 ...
		& PerformanceMem.Cond.OldNew==1)))=1;
	%MISSES
	submem(sort(PerformanceMem.Cond.EncTrial(PerformanceMem.Resp.ObjRec>3 ...
		& PerformanceMem.Cond.OldNew==1)))=0;
catch
	warning('Memory Data Missing!')
	pwd
end

runs=unique(Performance.cond.Run);
runs=runs(runs>0);
%loop through each run and make 3 column FSL EV files
for r = runs
	
	%get EV onset times for each run
	choice_time = Performance.time.startTrial(valid_trials & Performance.cond.Run==r)'-dels;
	response_time = Performance.time.startChoice(valid_trials & Performance.cond.Run==r)'-dels;
	FB_time = Performance.time.startFB(valid_trials & Performance.cond.Run==r)'-dels;
	inval_time = Performance.time.startTrial(isnan(Performance.choose.resp(Performance.cond.Run==r)))'-dels;
	
	%get durations
	choice_duration = Performance.time.startChoice(valid_trials & Performance.cond.Run==r)'- Performance.time.startTrial(valid_trials & Performance.cond.Run==r)';
	response_duration = repmat(2,length(choice_time),1);
	%choice_duration = Performance.time.startDelay(valid_trials(Performance.cond.Run==r))'- Performance.time.startChoice(valid_trials & Performance.cond.Run==r)';
	FB_duration = Performance.time.startISI(valid_trials & Performance.cond.Run==r)'- Performance.time.startFB(valid_trials & Performance.cond.Run==r)';
	inval_duration = Performance.time.startISI(isnan(Performance.choose.resp(Performance.cond.Run==r)))'- Performance.time.startTrial(isnan(Performance.choose.resp(Performance.cond.Run==r)))';

	%get weights for parametric regressors
	FB_weight = Performance.pay.outcome(valid_trials & Performance.cond.Run==r)';

	%RL model regressors
	FB_pe_weight=N(N(:,1)==sub & N(:,2)==r,35);
	choice_Qdiff_weight=N(N(:,1)==sub & N(:,2)==r,34);
	choice_Qchose_weight=N(N(:,1)==sub & N(:,2)==r,33);

	%index old vs new choice
	old_choice=(N(N(:,1)==sub & N(:,2)==r,15))==.5;
	new_choice=(N(N(:,1)==sub & N(:,2)==r,15))==-.5;
	no_choice=(N(N(:,1)==sub & N(:,2)==r,15))==0;

    %combine into FSL-style 3col regs
    choice_run = [choice_time, choice_duration, ones(size(choice_time,1),1)];
    response_run = [response_time, response_duration, ones(size(response_time,1),1)];
    FBpay_run = [FB_time, FB_duration, FB_weight];
    FB_run = [FB_time, FB_duration, ones(size(FB_time,1),1)];
    inval_run = [inval_time, inval_duration, ones(size(inval_time,1),1)];

    choice_oldc=choice_run(old_choice,:);
    choice_newc=choice_run(new_choice,:);
    choice_noc=choice_run(no_choice,:);

    FB_oldc=FB_run(old_choice,:);
    FB_newc=FB_run(new_choice,:);
    FB_noc=FB_run(no_choice,:);

    FBpay_oldc=FBpay_run(old_choice,:);
    FBpay_newc=FBpay_run(new_choice,:);
    FBPay_noc=FBpay_run(no_choice,:);
	

	%episodic EVs
	if exist('PerformanceMem')
		%parametric weights
		oldnew_weight=~Performance.cond.oldnewobj(valid_trials & Performance.cond.Run==r);
		oldval_weight=Performance.cond.priorpay(valid_trials & Performance.cond.Run==r);
		oldval_weight=oldval_weight-.5;	
		submem_weight=submem(valid_trials & Performance.cond.Run==r);

		%combine and remove missed trials
		oldnew_run=[choice_run(:,1:2) oldnew_weight'];	
		oldval_run=[choice_run(:,1:2) oldval_weight'];
		submem_run=[choice_run(:,1:2) submem_weight'];
		oldval_oldc=oldval_run(old_choice,:);
		oldval_newc=oldval_run(new_choice,:);

		oldnew_run(oldnew_run(:,1)<0,:)=[];
		oldval_run(oldval_run(:,1)<0 | isnan(oldval_run(:,3)),:)=[];
		oldval_oldc(oldval_oldc(:,1)<0 | isnan(oldval_oldc(:,3)),:)=[];
		oldval_newc(oldval_newc(:,1)<0 | isnan(oldval_newc(:,3)),:)=[];
		submem_run(submem_run(:,1)<0 | isnan(submem_run(:,3)),:)=[];

		%write out EV txt files for episodic predictors
		dlmwrite(strcat('EV_files/oldnew_run',num2str(r),'.txt'),oldnew_run, 'delimiter',' ');
		dlmwrite(strcat('EV_files/oldval_run',num2str(r),'.txt'),oldval_run, 'delimiter',' ');
		dlmwrite(strcat('EV_files/oldval_oldc_run',num2str(r),'.txt'),oldval_oldc, 'delimiter',' ');
		dlmwrite(strcat('EV_files/oldval_newc_run',num2str(r),'.txt'),oldval_newc, 'delimiter',' ');
		dlmwrite(strcat('EV_files/submem_run',num2str(r),'.txt'),submem_run, 'delimiter',' ');
	end

	%if there is RL model output for subject/run make and write EV files 
	if size(FB_pe_weight,1)>0
		FB_pe_run = [FB_time, FB_duration, FB_pe_weight(valid_trials(Performance.cond.Run==r))];
		choice_Qdiff_run=[choice_time, choice_duration, choice_Qdiff_weight(valid_trials(Performance.cond.Run==r))];
		choice_Qchose_run=[choice_time, choice_duration, choice_Qchose_weight(valid_trials(Performance.cond.Run==r))];

		FB_pe_oldc=FB_pe_run(old_choice,:);
		FB_pe_newc=FB_pe_run(new_choice,:);
		FB_pe_noc=FB_pe_run(no_choice,:);
		choice_Qdiff_oldc=choice_Qdiff_run(old_choice,:);
		choice_Qdiff_newc=choice_Qdiff_run(new_choice,:);
		choice_Qdiff_noc=choice_Qdiff_run(no_choice,:);
		choice_Qchose_oldc=choice_Qchose_run(old_choice,:);
		choice_Qchose_newc=choice_Qchose_run(new_choice,:);
		choice_Qchose_noc=choice_Qchose_run(no_choice,:);

		
		FB_pe_run(FB_pe_run(:,1)<0 | isnan(FB_pe_run(:,3)),:)=[];
		choice_Qdiff_run(choice_Qdiff_run(:,1)<0 | isnan(choice_Qdiff_run(:,3)),:)=[];
		choice_Qchose_run(choice_Qchoose_run(:,1)<0 | isnan(choice_Qchose_run(:,3)),:)=[];
		FB_pe_oldc(FB_pe_oldc(:,1)<0 | isnan(FB_pe_oldc(:,3)),:)=[];
		FB_pe_newc(FB_pe_newc(:,1)<0 | isnan(FB_pe_newc(:,3)),:)=[];
		FB_pe_noc(FB_pe_noc(:,1)<0 | isnan(FB_pe_noc(:,3)),:)=[];
		choice_Qdiff_oldc(choice_Qdiff_oldc(:,1)<0 | isnan(choice_Qdiff_oldc(:,3)),:)=[];
		choice_Qdiff_newc(choice_Qdiff_newc(:,1)<0 | isnan(choice_Qdiff_newc(:,3)),:)=[];
		choice_Qdiff_noc(choice_Qdiff_noc(:,1)<0 | isnan(choice_Qdiff_noc(:,3)),:)=[];
		choice_Qchose_oldc(choice_Qchose_oldc(:,1)<0 | isnan(choice_Qchose_oldc(:,3)),:)=[];
		choice_Qchose_newc(choice_Qchose_newc(:,1)<0 | isnan(choice_Qchose_newc(:,3)),:)=[];
		choice_Qchose_noc(choice_Qchose_noc(:,1)<0 | isnan(choice_Qchose_noc(:,3)),:)=[];;


		dlmwrite(strcat('EV_files/FB_pe_run',num2str(r),'.txt'),FB_pe_run, 'delimiter',' ');
		dlmwrite(strcat('EV_files/choice_Qdiff_run',num2str(r),'.txt'),choice_Qdiff_run, 'delimiter',' ');
		dlmwrite(strcat('EV_files/choice_Qchose_run',num2str(r),'.txt'),choice_Qchose_run, 'delimiter',' ');
		
		dlmwrite(strcat('EV_files/FB_pe_oldc_run',num2str(r),'.txt'),FB_pe_oldc, 'delimiter',' ');
		dlmwrite(strcat('EV_files/FB_pe_newc_run',num2str(r),'.txt'),FB_pe_newc, 'delimiter',' ');
		dlmwrite(strcat('EV_files/FB_pe_noc_run',num2str(r),'.txt'),FB_pe_noc, 'delimiter',' ');

		dlmwrite(strcat('EV_files/choice_Qdiff_oldc_run',num2str(r),'.txt'),choice_Qdiff_oldc, 'delimiter',' ');
		dlmwrite(strcat('EV_files/choice_Qdiff_newc_run',num2str(r),'.txt'),choice_Qdiff_newc, 'delimiter',' ');
		dlmwrite(strcat('EV_files/choice_Qdiff_noc_run',num2str(r),'.txt'),choice_Qdiff_noc, 'delimiter',' ');

		dlmwrite(strcat('EV_files/choice_Qchose_oldc_run',num2str(r),'.txt'),choice_Qchose_oldc, 'delimiter',' ');
		dlmwrite(strcat('EV_files/choice_Qchose_newc_run',num2str(r),'.txt'),choice_Qchose_newc, 'delimiter',' ');
		dlmwrite(strcat('EV_files/choice_Qchose_noc_run',num2str(r),'.txt'),choice_Qchose_noc, 'delimiter',' ');

	end

	%LEFT OFF HERE. ALSO NEED TO GO BACK AND FIX TIMINGS FOR old choice files
	%remove trials from deleted volumes
	choice_run(choice_run(:,1)<0,:)=[];
	response_run(response_run(:,1)<0,:)=[];
	FBpay_run(FBpay_run(:,1)<0,:)=[];
	FB_run(FB_run(:,1)<0,:)=[];
	inval_run(inval_run(:,1)<0,:)=[];

	choice_oldc(choice_oldc(:,1)<0,:)=[];
    choice_newc(choice_newc(:,1)<0,:)=[];
    choice_noc(choice_noc(:,1)<0,:)=[];

    FB_oldc(FB_oldc(:,1)<0,:)=[];
    FB_newc(FB_newc(:,1)<0,:)=[];
    FB_noc(FB_noc(:,1)<0,:)=[];

    FBpay_oldc(FBpay_oldc(:,1)<0,:)=[];
    FBpay_newc(FBpay_newc(:,1)<0,:)=[];
    FBPay_noc(FBpay_noc(:,1)<0,:)=[];


	
	if size(inval_run,1)==0
		inval_run=[0, 0, 0];
	end
		

	%write out txt files
	dlmwrite(strcat('EV_files/choice_run',num2str(r),'.txt'), choice_run, 'delimiter',' ');
	dlmwrite(strcat('EV_files/response_run',num2str(r),'.txt'), response_run, 'delimiter',' ');
	dlmwrite(strcat('EV_files/FB_run',num2str(r),'.txt'), FB_run, 'delimiter',' ');
	dlmwrite(strcat('EV_files/FBpay_run',num2str(r),'.txt'),FBpay_run, 'delimiter',' ');
	dlmwrite(strcat('EV_files/inval_run',num2str(r),'.txt'),inval_run, 'delimiter',' ');

	dlmwrite(strcat('EV_files/choice_oldc_run',num2str(r),'.txt'), choice_oldc, 'delimiter',' ');
    dlmwrite(strcat('EV_files/choice_newc_run',num2str(r),'.txt'), choice_newc, 'delimiter',' ');
    dlmwrite(strcat('EV_files/choice_noc_run',num2str(r),'.txt'), choice_noc, 'delimiter',' ');

    dlmwrite(strcat('EV_files/FB_oldc_run',num2str(r),'.txt'), FB_oldc, 'delimiter',' ');
    dlmwrite(strcat('EV_files/FB_newc_run',num2str(r),'.txt'), FB_newc, 'delimiter',' ');
    dlmwrite(strcat('EV_files/FB_noc_run',num2str(r),'.txt'), FB_noc, 'delimiter',' ');

    dlmwrite(strcat('EV_files/FBpay_oldc_run',num2str(r),'.txt'),FBpay_oldc, 'delimiter',' ');
    dlmwrite(strcat('EV_files/FBpay_newc_run',num2str(r),'.txt'),FBpay_newc, 'delimiter',' ');
    dlmwrite(strcat('EV_files/FBpay_noc_run',num2str(r),'.txt'),FBpay_noc, 'delimiter',' ');
end


end


