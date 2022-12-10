program main
   use iso_fortran_env
   use aoc_fortran
   implicit none
   character(10)::str
   integer::regx,loop,x,num
   integer::th,ios,i
   character::pic(40,6)="0"
   open(10,file="data/10.txt")
   regx=1
   loop=0
   num=0
   th=20
   do
      read(10,"(A)",iostat=ios)str
      if(is_iostat_end(ios))exit
      select case(str(1:4))
      case("noop")
         loop=loop+1
         if(loop==th)then
            num=num+th*regx
            th=th+40
         end if
      case("addx")
         do i=1,2
            loop=loop+1
            if(loop==th)then
               num=num+th*regx
               th=th+40
            end if
         end do
         x=tonum(str(5:))
         regx=regx+x
      end select
   end do
   write(*,*)num
   rewind(10)
   regx=1
   loop=0
   num=1
   th=40
   do
      read(10,"(A)",iostat=ios)str
      if(is_iostat_end(ios))exit
      select case(str(1:4))
      case("noop")
         loop=loop+1
         pic(loop,num)=merge("#",".",regx<=loop.and.loop<=regx+2)
         if(loop==th)then
            loop=0
            num=num+1
         end if
      case("addx")
         do i=1,2
            loop=loop+1
            pic(loop,num)=merge("#",".",regx<=loop.and.loop<=regx+2)
            if(loop==th)then
               loop=0
               num=num+1
            end if
         end do
         x=tonum(str(5:))
         regx=regx+x
      end select
   end do
   write(*,"(40A1)")pic
end program main
