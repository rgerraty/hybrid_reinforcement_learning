function [runs]=behavioral_colums_works(dels)

%dels is seconds of deleted fmri volumes from beginning of scan
%note that right now you need to be folder containing subjects' .mat files. may want to change this

addpath /home/rgerraty/scripts/MATLAB/

[M,N]=csvreadh('/data/engine/rgerraty/hybrid_mri/behavior/hybrid_data.csv',',');


%only need to load last run's file
%[status,runs]=unix('ls Performance_?.mat | wc -l');
[status,runs]=unix('ls Performance_?.mat | cut -c13');

runs=str2num(runs);

[status,sub]=unix('pwd | cut -c43-44');
sub=str2num(sub);

%try loading last Performance file but quit if any discrepencies
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
	submem(sort(PerformanceMem.Cond.EncTrial(PerformanceMem.Resp.ObjRec<3 & PerformanceMem.Cond.OldNew==1)))=1;
	%MISSES
	submem(sort(PerformanceMem.Cond.EncTrial(PerformanceMem.Resp.ObjRec>3 & PerformanceMem.Cond.OldNew==1)))=0;
catch
	warning('Memory Data Missing!')
	pwd
end


for i = 1:size(runs,1)
	r=runs(i);
	
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

	FB_pe_weight=N(N(:,1)==sub & N(:,2)==r,32);
	choice_Qdiff_weight=N(N(:,1)==sub & N(:,2)==r,31);
	choice_Qchose_weight=N(N(:,1)==sub & N(:,2)==r,30);

    	%combine into FSL-style 3col regs
    	choice_run = [choice_time, choice_duration, ones(size(choice_time,1),1)];
	
    	response_run = [response_time, response_duration, ones(size(response_time,1),1)];
	
    	FBpay_run = [FB_time, FB_duration, FB_weight];
	
    	FB_run = [FB_time, FB_duration, ones(size(FB_time,1),1)];
	
    	inval_run = [inval_time, inval_duration, ones(size(inval_time,1),1)];
	

	%episodic EVs
	if exist('PerformanceMem')
		oldnew_weight=~Performance.cond.oldnewobj(valid_trials & Performance.cond.Run==r);
		oldnew_run=[choice_run(:,1:2) oldnew_weight'];
		
		oldval_weight=Performance.cond.priorpay(valid_trials & Performance.cond.Run==r);
		oldval_weight=oldval_weight-.5;		
		oldval_run=[choice_run(:,1:2) oldval_weight'];
		
		submem_weight=submem(valid_trials & Performance.cond.Run==r);
		submem_run=[choice_run(:,1:2) submem_weight'];
		
		oldnew_run(choice_time<0,:)=[];
		oldval_run(choice_time<0 | isnan(oldval_run(:,3)),:)=[];
		
		submem_run(choice_time<0 | isnan(submem_run(:,3)),:)=[];

		dlmwrite(strcat('EV_files/oldnew_run',num2str(r),'.txt'),oldnew_run, 'delimiter',' ');
		dlmwrite(strcat('EV_files/oldval_run',num2str(r),'.txt'),oldval_run, 'delimiter',' ');
		dlmwrite(strcat('EV_files/submem_run',num2str(r),'.txt'),submem_run, 'delimiter',' ');
	end
		
	%remove trials from deleted volumes
	choice_run(choice_time<0,:)=[];
	response_run(response_time<0,:)=[];
	FBpay_run(FB_time<0,:)=[];
	FB_run(FB_time<0,:)=[];
	inval_run(inval_time<0,:)=[];
	
	if size(inval_run,1)==0
		inval_run=[0, 0, 0];
	end
		
	
	
	
	
	%write
	dlmwrite(strcat('EV_files/choice_run',num2str(r),'.txt'), choice_run, 'delimiter',' ');
	dlmwrite(strcat('EV_files/response_run',num2str(r),'.txt'), response_run, 'delimiter',' ');
	dlmwrite(strcat('EV_files/FB_run',num2str(r),'.txt'), FB_run, 'delimiter',' ');
	dlmwrite(strcat('EV_files/FBpay_run',num2str(r),'.txt'),FBpay_run, 'delimiter',' ');
	dlmwrite(strcat('EV_files/inval_run',num2str(r),'.txt'),inval_run, 'delimiter',' ');

	if size(FB_pe_weight,1)>0
		FB_pe_run = [FB_time, FB_duration, FB_pe_weight(valid_trials(Performance.cond.Run==r))];
		choice_Qdiff_run=[choice_time, choice_duration, choice_Qdiff_weight(valid_trials(Performance.cond.Run==r))];
		choice_Qchose_run=[choice_time, choice_duration, choice_Qchose_weight(valid_trials(Performance.cond.Run==r))];
		
		FB_pe_run(FB_time<0,:)=[];
		choice_Qdiff_run(choice_time<0,:)=[];
		choice_Qchose_run(choice_time<0,:)=[];

		dlmwrite(strcat('EV_files/FB_pe_run',num2str(r),'.txt'),FB_pe_run, 'delimiter',' ');
		dlmwrite(strcat('EV_files/choice_Qdiff_run',num2str(r),'.txt'),choice_Qdiff_run, 'delimiter',' ');
		dlmwrite(strcat('EV_files/choice_Qchose_run',num2str(r),'.txt'),choice_Qchose_run, 'delimiter',' ');
	end
end


end


