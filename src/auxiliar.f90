!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!	This file contains axiliary subroutines, mostly for reading/writting data
!!	Author:	Clara Estela Jimenez Tejero. 
!!	License: GNU General Public License v3.0
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

SUBROUTINE READ_DATA(iOBS,nSS,input)

USE mod_parfile
USE mod_data_arrays

IMPLICIT NONE

	INTEGER :: nh,iOBS
	INTEGER :: nSS,i,j,k,kk
	INTEGER(4) :: pos_r

	REAL(4) :: input(nt,nSS) 	
	REAL(4), ALLOCATABLE :: sudata(:,:) 		

	CHARACTER(len=50) :: access,form
	CHARACTER(len=500) :: file_name

	nh = size_su_header

	ALLOCATE(sudata(nt+nh,nSS))	!!nss: NumShots
	sudata=0.;

!	Loop trace by trace
	do j=1,nSS

		pos_r = pos_byte_su(j)
!		pos_r = 1 + (j-1)*(nh+nt)*4

		READ(unit0+iOBS, pos=pos_r)  sudata(1:nt+nh,j)	

	enddo

	do j=1,nSS

		do k=1,nt
			input(k,j)=sudata(k+nh,j)
		enddo

	enddo

	deallocate(sudata)

END SUBROUTINE READ_DATA

subroutine WRITE_INPUT(iOBS,nSS,SG)	!!paralelizar

USE mod_parfile
USE mod_data_arrays

implicit none

integer :: i,j,k,nh,it,iOBS
INTEGER :: nSS
INTEGER(4) :: pos_byte	

real :: SG(nt,nSS)
REAL(4), ALLOCATABLE :: sudata(:,:)

CHARACTER(len=500) :: file_name,Str,Str_DC

nh = size_su_header

ALLOCATE(sudata(nh,nSS))
sudata=0.;

! Loop trace by trace

do j=1,nSS	!!nSS: NumShots

	pos_byte = pos_byte_su(j)

	READ(unit0+iOBS, pos=pos_byte)  sudata(1:nh,j)	!! read data

enddo

do j=1,nSS

	pos_byte = pos_byte_su(j)

	WRITE(unit_DC0+iOBS, pos=pos_byte)  sudata(1:nh,j)	!! write header
        WRITE(unit_DC0+iOBS, pos=pos_byte+nh*4) SG(1:nt,j)	!! real traces

enddo

deallocate(sudata)

END SUBROUTINE WRITE_INPUT

subroutine WRITE_OUTPUT(iOBS,nSS,SG)	!!paralelizar

USE mod_parfile
USE mod_data_arrays

implicit none

integer :: i,j,k,nh,it,iOBS
INTEGER :: nSS
INTEGER(4) :: pos_byte	

real :: SG(nt,nSS)
REAL(4), ALLOCATABLE :: sudata(:,:),shot(:,:)

CHARACTER(len=500) :: file_name,Str,Str_DC

nh = size_su_header

ALLOCATE(sudata(nh,nSS))
ALLOCATE(shot(nt,nSS))
sudata=0.;shot=0.;

! Loop trace by trace

do j=1,nSS	!!nSS: NumShots

	pos_byte = pos_byte_su(j)

	READ(unit0+iOBS, pos=pos_byte)  sudata(1:nh,j)	!! read data

	do k=1,nt
		shot(k,j)=SG(nt-k+1,j)
	enddo

enddo

do j=1,nSS

	pos_byte = pos_byte_su(j)

	WRITE(unit_DC+iOBS, pos=pos_byte)  sudata(1:nh,j)	!! write header
        WRITE(unit_DC+iOBS, pos=pos_byte+nh*4) shot(1:nt,j)	!! real traces

enddo

deallocate(sudata)
deallocate(shot)

END SUBROUTINE WRITE_OUTPUT

subroutine SAVE_OBS_TXT(iOBS)

USE mod_parfile
USE mod_model_arrays
USE mod_data_arrays

implicit none

integer :: j,k,nh,iOBS
integer :: unit_output
integer(4) :: pos_byte

character(len=50) :: Str,Str_txt,OBS_num
character(len=500) :: file_name,file_name2,file_name3

REAL(4), ALLOCATABLE :: sudata(:,:),SG(:,:)

nh=size_su_header

allocate(sudata(nt+nh,NumShots))
allocate(SG(nt,NumShots))
SG=0.;sudata=0.;

if(save_matlab.ne.0)Str= 'gmt_'
if(save_gmt.ne.0)   Str= 'matlab_'

file_name = trim(adjustl(folder_output)) // trim(adjustl(Str))  // trim(adjustl(original_file(iOBS))) // '.txt'

do j=1,NumShots

	pos_byte = pos_byte_su(j)
!	pos_byte = 1 + (j-1)*(nh+nt)*4

	READ(unit_DC+iOBS,pos=pos_byte) sudata(1:nh+nt,j)

        do k=1,nt
		SG(k,j)=sudata(k+nh,j)
	enddo

enddo

if(save_matlab.ne.0)	then

	open(12,FILE=file_name,STATUS='unknown')
       	do k=1,nt
		write(12,'(20000(e12.5,2x))') (SG(k,j),j=1,NumShots)
	enddo
	close(12)

endif	!matlab

if(save_gmt.ne.0)	then

	open(12,FILE=file_name,STATUS='unknown')
	do j=1,NumShots
		do k=1,nt
			write(12,*)j,SG(k,j)
		enddo
	enddo
	close(12)

endif	!gmt


deallocate(SG)
deallocate(sudata)

end subroutine SAVE_OBS_TXT

SUBROUTINE ascii_art(i)

USE mod_parfile

implicit none


integer :: i

if(i.eq.1)	then

write(*,*)"	______                                         _ 	"
write(*,*)"	|  _  \                                       | |	"
write(*,*)"	| | | |_____      ___ ____      ____ _ _ __ __| |	"
write(*,*)"	| | | / _ \ \ /\ / / '_ \ \ /\ / / _` | '__/ _` |	"
write(*,*)"	| |/ / (_) \ V  V /| | | \ V  V / (_| | | | (_| |	"
write(*,*)"	|___/ \___/ \_/\_/ |_| |_|\_/\_/ \__,_|_|  \__,_|	"                                                             
write(*,*)                                                             
write(*,*)"	 _____             _   _                   _   _		"
write(*,*)"	/  __ \           | | (_)                 | | (_)            	"
write(*,*)"	| /  \/ ___  _ __ | |_ _ _ __  _   _  __ _| |_ _  ___  _ __	"
write(*,*)"	| |    / _ \| '_ \| __| | '_ \| | | |/ _` | __| |/ _ \| '_ \	"
write(*,*)"	| \__/\ (_) | | | | |_| | | | | |_| | (_| | |_| | (_) | | | |	"
write(*,*)"	 \____/\___/|_| |_|\__|_|_| |_|\__,_|\__,_|\__|_|\___/|_| |_|	"
write(*,*)
write(*,*)
write(*,*)
write(*,*)"	   ___    ___   ___     ___      _     _____     _   "
write(*,*)"	  / _ \  | _ ) / __|   |   \    /_\   |_   _|   /_\  "
write(*,*)"	 | (_) | | _ \ \__ \   | |) |  / _ \    | |    / _ \ "
write(*,*)"	  \___/  |___/ |___/   |___/  /_/ \_\   |_|   /_/ \_\"
write(*,*)
write(*,*)
write(*,*)                                                                      
write(*,*)"		Release (2022)			"
write(*,*)"		Author: Clara Estela Jiménez Tejero	"
write(*,*)"		email: ejimenez@icm.csic.es 		"
write(*,*)"		Barcelona Center for Subsurface Imaging "
write(*,*)"		Instituto de Ciencias Marinas (ICM-CSIC)"
write(*,*)
write(*,*)
write(*,*)"	                     |				"
write(*,*)"	                     |				"
write(*,*)"	            |        |				"
write(*,*)"	          |-|-|      |				"
write(*,*)"	            |        |				"
write(*,*)"	            | {O}    |				"
write(*,*)"	            '--|     |				"
write(*,*)"	              .|]_   |				"
write(*,*)"	        _.-=.' |     |				"	
write(*,*)"	       |    |  |]_   |				"
write(*,*)"	       |_.-='  |   __|__				"
write(*,*)"	        _.-='  |\   /|\				"
write(*,*)"	       |    |  |-'-'-'-'-.				"
write(*,*)"	       |_.-='  '========='				"
write(*,*)"	            `   |     |				"
write(*,*)"	             `. |    / \				"
write(*,*)"	               ||   /   \____.--=''''==--.._		"
write(*,*)"	               ||_.'--=='    |__  __  __  _.'	"
write(*,*)"	               ||  |    |    |\ ||  ||  || |                        ___	"	
write(*,*)"	  ____         ||__|____|____| \||__||__||_/    __________________/|   |	"
write(*,*)"	 |    |______  |===.---. .---.========''''=-._ |     |     |     / |   |	"
write(*,*)"	 |    ||     |\| |||   | |   |      '===' ||  \|_____|_____|____/__|___|	"
write(*,*)"	 |-.._||_____|_\___'---' '---'______....---===''======//=//////========|	"
write(*,*)"	 |--------------\------------------/-----------------//-//////---------/	"
write(*,*)"	 |               \                /                 // //////         /	"
write(*,*)"	 |                \______________/                 // //////         /	"
write(*,*)"	 |                                        _____===//=//////=========/	"
write(*,*)"	 |=================================================================/		"
write(*,*)"	  -----------------------------------------------------------------		"
write(*,*)"	``'-.,_,.-'``'-.,_,.='``'-.,_,.-'``'-.,_,.='````'-.,_,.-'``'-.,_,.='``'-.		"
write(*,*)"	``'-.,_,.-'``'-.,_,.='``'-.,_,.-'``'-.,_,.='````'-.,_,.-'``'-.,_,.='``'-.		"
write(*,*)"	``'-.,_,.-'``'-.,_,.='``'-.,_,.-'``'-.,_,.='````'-.,_,.-'``'-.,_,.='``'-.		"
write(*,*)
write(*,*)	
write(*,*)"				         ______		"
write(*,*)"				        /      \ 	"	
write(*,*)"				       /        \ 	"
write(*,*)"				       |        |	"
write(*,*)"				    )  o        o   (	"
write(*,*)"				   (    \      /    )	"
write(*,*)"				  _ \___/||||||\___/ _	"
write(*,*)"				   \____/ |||| \____/ `	"
write(*,*)"				   ,-.___/ || \__,-._	"
write(*,*)"				  /    ___/  \__	"
write(*,*)"				     _/         `--	"
write(*,*)
write(*,*)
write(*,*)"	``'-.,_,.-'``'-.,_,.='``'-.,_,.-'``'-.,_,.='````'-.,_,.-'``'-.,_,.='``'-.		"
write(*,*)"	``'-.,_,.-'``'-.,_,.='``'-.,_,.-'``'-.,_,.='````'-.,_,.-'``'-.,_,.='``'-.		"
write(*,*)"	``'-.,_,.-'``'-.,_,.='``'-.,_,.-'``'-.,_,.='````'-.,_,.-'``'-.,_,.='``'-.		"
write(*,*)
write(*,*)


endif

if(i.eq.2)	then

write(*,*)
write(*,*)
write(*,*)'Fight your bug'
write(*,*)'                                |     |'
write(*,*)'                                \\_V_//'
write(*,*)'                                \/=|=\/'
write(*,*)'                                 [=v=]'
write(*,*)'                               __\___/_____'
write(*,*)'                              /..[  _____  ]'
write(*,*)'                             /_  [ [  M /] ]'
write(*,*)'                            /../.[ [ M /@] ]'
write(*,*)'                           <-->[_[ [M /@/] ]'
write(*,*)'                          /../ [.[ [ /@/ ] ]'
write(*,*)'     _________________]\ /__/  [_[ [/@/ C] ]'
write(*,*)'    <_________________>>0---]  [=\ \@/ C / /'
write(*,*)'       ___      ___   ]/000o   /__\ \ C / /'
write(*,*)'          \    /              /....\ \_/ /'
write(*,*)'       ....\||/....           [___/=\___/'
write(*,*)'      .    .  .    .          [...] [...]'
write(*,*)'     .      ..      .         [___/ \___]'
write(*,*)'     .    0 .. 0    .         <---> <--->'
write(*,*)'  /\/\.    .  .    ./\/\      [..]   [..]'
write(*,*)' / / / .../|  |\... \ \ \    _[__]   [__]_'
write(*,*)'/ / /       \/       \ \ \  [____>   <____]'
write(*,*)
write(*,*)

endif

if(i.eq.3)	then

	write(*,*)
	write(*,*)
	write(*,*)
	write(*,*)
	write(*,*)"	 _______  ___   __    _  ___   _______  __   __  _______  ______  	"
	write(*,*)"	|       ||   | |  |  | ||   | |       ||  | |  ||       ||      | 	"
	write(*,*)"	|    ___||   | |   |_| ||   | |  _____||  |_|  ||    ___||  _    |	"
	write(*,*)"	|   |___ |   | |       ||   | | |_____ |       ||   |___ | | |   |	"
	write(*,*)"	|    ___||   | |  _    ||   | |_____  ||       ||    ___|| |_|   |	"
	write(*,*)"	|   |    |   | | | |   ||   |  _____| ||   _   ||   |___ |       |	"
	write(*,*)"	|___|    |___| |_|  |__||___| |_______||__| |__||_______||______|	"
	write(*,*)
	write(*,*)

endif

if(i.eq.4)	then

write(*,*)""
write(*,*)""
write(*,*)"*** WARNING: numtasks do not need to be greater than variable NumOBS"
write(*,*)"***          You are wasting energy. Please, next time you run a job, set in you MPI execution line, numtasks=",NumOBS
write(*,*)""
write(*,*)" SMALL DECISIONS CAN SAVE OUR PLANET "
write(*,*)"             _____"
write(*,*)"          .-'.  ':'-."
write(*,*)"        .''::: .:    '."
write(*,*)"       /   :::::'      \"
write(*,*)"      ;.    ':' `       ;"
write(*,*)"      |       '..       |"
write(*,*)"      ; '      ::::.    ;"
write(*,*)"       \       '::::   /"
write(*,*)"        '.      :::  .'"
write(*,*)"          '-.___'_.-'"
write(*,*)""

endif

END SUBROUTINE ascii_art

SUBROUTINE open_su_files(iOBS)

USE mod_parfile
USE mod_data_arrays

IMPLICIT NONE
integer	:: iOBS
CHARACTER(len=50) :: access,form,OBS_num
CHARACTER(len=500) :: file_name,file_name2,file_name3

access = 'stream'
form = 'unformatted'

file_name = trim(adjustl(folder_input)) // original_file(iOBS)

if(endianness_machine.eq.0)	then

	if(endianness_data.eq.0)open(unit0+iOBS,FILE=file_name,ACCESS=access,FORM=form,STATUS='old')
	if(endianness_data.eq.1)open(unit0+iOBS,FILE=file_name,ACCESS=access,FORM=form,CONVERT='BIG_ENDIAN',STATUS='old')

endif

if(endianness_machine.eq.1)	then

	if(endianness_data.eq.0)open(unit0+iOBS,FILE=file_name,ACCESS=access,FORM=form,CONVERT='LITTLE_ENDIAN',STATUS='old')
	if(endianness_data.eq.1)open(unit0+iOBS,FILE=file_name,ACCESS=access,FORM=form,STATUS='old')

endif

INQUIRE(FILE=file_name, SIZE=sizeof)

write(OBS_num,*) iOBS

file_name = trim(adjustl(folder_output)) // 'DC_' // trim(adjustl(original_file(iOBS)))
open(unit_DC+iOBS,FILE=file_name,ACCESS=access,FORM=form,CONVERT='BIG_ENDIAN',STATUS='unknown')

if(filtering.ne.0)	then
	file_name = trim(adjustl(folder_output)) // 'Filtered_' // trim(adjustl(original_file(iOBS)))
	open(unit_DC0+iOBS,FILE=file_name,ACCESS=access,FORM=form,CONVERT='BIG_ENDIAN',STATUS='unknown')
endif

!maxbytes=maxval(sizeof(:))

END SUBROUTINE open_su_files

subroutine close_su_files(iOBS)

USE mod_parfile
implicit none

integer :: iOBS

close(unit0+iOBS)
close(unit_DC+iOBS)

if(filtering.ne.0)close(unit_DC0+iOBS)

end subroutine close_su_files


subroutine time_filter(Data_trace,nt,dt,Num,f1,fc)

implicit none

        integer :: nt,Num,nt2,zero_pad,ind,typef
        real :: dt,fe,var,fc,f1
        real :: Data_trace(nt,Num)
        real,allocatable :: tmp(:)
        integer :: j,i,k
        logical :: isnotCero            ! Variable para almacenar si el vector es cero

        typef=3
        zero_pad=24
        nt2=zero_pad*nt
        allocate(tmp(nt2))
        fe=1/dt

        do i=1,Num

                tmp=0.
                tmp((zero_pad-1)*nt+1:nt2)=Data_trace(:,i)

                call is_not_cero(nt,Data_trace(:,i),isnotCero)

                if(isnotCero)   then

!                       do j=1,nt
!                               write(55,*)j,Data_trace(j,i)
!                       enddo
!                        call filters(tmp,nt2,fe,1,typef,f1,fc)
!                        Data_trace(:,i)=tmp(nt2-nt+1:nt2)


                endif

        enddo


        deallocate(tmp)

return
end


subroutine is_not_cero(nt,data1d,isnotCero)
implicit none

    integer :: i,nt
    real :: data1d(nt)
    logical :: isnotCero

    isnotCero=.FALSE.

    ! Verificar si todas las componentes son cero
    do i = 1, nt
        if (data1d(i) /= 0.0) then
            isnotCero = .TRUE.
            exit ! Puedes salir del bucle en cuanto encuentres un valor no cero
        end if
    end do

end subroutine is_not_cero
