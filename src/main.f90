!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!	This is the main file for the calculation of Downward Continuation
!!	Author:	Clara Estela Jimenez Tejero. 
!!	License: GNU General Public License v3.0
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

PROGRAM main

USE mod_parfile
USE mod_data_arrays
USE mod_model_arrays

implicit none
include 'mpif.h'

integer :: numtasks,rank,ierr,status(MPI_STATUS_SIZE)
integer :: itimes,ntimes,iOBS
integer :: iDC,corr

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

call MPI_INIT(ierr)
call MPI_COMM_SIZE(MPI_COMM_WORLD,numtasks,ierr)
call MPI_COMM_RANK(MPI_COMM_WORLD,rank,ierr)
ierr=0

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!	ASCII ART WELLCOME MESSAGE
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

if(rank.eq.0)	then
	call ascii_art(1)
       	write(*,*)
endif

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!     Lectura de parametros Par_file
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

call read_parfile(rank)
call allocate_data_arrays()

if(numtasks.gt.NumOBS)  then

        if(iOBS.eq.1)call ascii_art(4)

endif

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!     MAIN LOOP DC
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

ntimes=NumOBS
if(numtasks.gt.1)	then

        if(NumOBS.gt.numtasks)        then
                ntimes=ceiling(1.*NumOBS/numtasks)
        endif

	if(NumOBS.le.numtasks)        then
                ntimes=1
        endif

endif

if(numtasks.eq.1.and.ntimes.gt.1) write(*,*)'NO PARALELIZATION'

do itimes=1,ntimes

iOBS=(itimes-1)*numtasks+rank+1        !!here parallelization

if(iOBS.le.NumOBS) then

	call inputs(iOBS,rank,numtasks)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!	WRITE STREAMER DATA TO DC IN OUTPUT FOLDER
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

	if(rank.eq.0)	then
		write(*,*)
		write(*,*)'*******************************************'
		write(*,*)'PREPARING AND WRITING INPUT DATA, STEP 0'
		write(*,*)'*******************************************'
	endif

	iDC=0
	if(input_rewrite.ne.0)	then
		call WRITE_INPUT(iOBS)
		if(save_txt.ne.0) then
			corr=0
			if(rank.eq.0)write(*,*)'SAVING TXT DATA'
			call SAVE_OBS_TXT(iDC,iOBS,corr)
		endif
	endif

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!! START DC
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

	if(rank.eq.0)	then

		write(*,*)
		write(*,*)"``'-.,_,.-'``'-.,_,.='``'-.,_,.-'``'-.,_,.='````'-.,_,.-'``'-.,_,.='``'-."
		write(*,*)"``'-.,_,.-'``'-.,_,.='``'-.,_,.-'``'-.,_,.='````'-.,_,.-'``'-.,_,.='``'-."
		write(*,*)"``'-.,_,.-'``'-.,_,.='``'-.,_,.-'``'-.,_,.='````'-.,_,.-'``'-.,_,.='``'-."

		write(*,*)
		write(*,*)'*******************************************'
		write(*,*)'DOWNWARD CONTINUATION'
		write(*,*)'*******************************************'

		write(*,*)
		write(*,*)'>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>'
		write(*,*)
		write(*,*)'DC for OBSs: redatuming sources from datum 1 to datum 2'
		write(*,*)'TOTAL NUMBER OF ROUNDS: ',ntimes
		write(*,*)'ROUND: ',itimes
		write(*,*)
		write(*,*)'<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<'

	endif

	iDC=1
	call DC_function(iDC,iOBS,rank,datum1,datum2)

        if(save_txt.eq.1) then
		corr=0
		if(rank.eq.0)write(*,*)'SAVING TXT DATA'
        	call SAVE_OBS_TXT(iDC,iOBS,corr)
       	endif


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!! 			END DOWNWARD CONTINUATION
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

	if(rank.eq.0.and.itimes.eq.ntimes)call ascii_art(3)

	call deallocate_model_arrays
	call close_su_files(iOBS)

endif	!iOBS<=NumOBS
enddo	!itimes

call deallocate_data_arrays

call MPI_FINALIZE(ierr)

end program main
