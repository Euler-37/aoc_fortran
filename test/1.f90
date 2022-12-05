program main
   use fortran_vector
   use iso_fortran_env
   implicit none
   character(len=10)::s=""
   integer::res,a,b
   integer::ios
   type(vector_int)::v
   open(10,file="data/1.txt")
   res=0
   a=0
   call v%init()
   do
      read(10,"(A10)",iostat=ios)s
      if(is_iostat_end(ios))exit
      if(s=="")then
         call v%append(a)
         a=0
      else
         read(s,*)b
         a=a+b
      end if
   end do
   call v%cut()
   call v%sort()
   associate(n=>size(v))
      write(*,*)v%x_(n)
      write(*,*)sum(v%x_(n-2:n))
   end associate
end program main
