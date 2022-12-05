program main
   use aoc_fortran
   use iso_fortran_env
   implicit none
   character(len=100)::buf
   integer::ios
   integer::a(4),fully,overlap,lines
   fully=0
   overlap=0
   lines=0
   open(10,file="data/4.txt")
   do
      read(10,"(A)",iostat=ios)buf
      if(is_iostat_end(ios))exit
      lines=lines+1
      call replace(buf,'-',',')
      read(buf,*)a
      if((a(3)<=a(1).and.a(4)>=a(2)).or.(a(3)>=a(1).and.a(4)<=a(2)))then
         fully=fully+1
      end if
      if((a(1)>a(4)).or.(a(2)<a(3)))then
         overlap=overlap+1
      end if
   end do
   write(*,*)fully
   write(*,*)lines-overlap
   close(10)
end program main
