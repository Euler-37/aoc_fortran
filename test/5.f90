program main
   use stack_mod
   use iso_fortran_env
   implicit none
   type(stack)::a(9),tmp
   character::str(9),t
   integer::ios,i,n1,n2,n3
   call a%init(100)
   open(10,file="data/5.txt")
   do
      read(10,"(1x,9(A1,3X))",iostat=ios)str
      if(str(1)=="1")exit
      do i=1,9
         if(str(i)/=" ") call a(i)%push(str(i))
      end do
   end do
   call a%reverse()
   do
      read(10,*,iostat=ios)t,n1,t,n2,t,n3
      if(is_iostat_end(ios))exit
      do i=1,n1
         call a(n3)%push(a(n2)%pop())
      end do
   end do
   write(*,"(*(g0))")(a(i)%top(),i=1,9)
   call a%clear()
   rewind(10)
   call a%init(100)
   open(10,file="data/5.txt")
   do
      read(10,"(1x,9(A1,3X))",iostat=ios)str
      if(str(1)=="1")exit
      do i=1,9
         if(str(i)/=" ") call a(i)%push(str(i))
      end do
   end do
   call a%reverse()
   do
      read(10,*,iostat=ios)t,n1,t,n2,t,n3
      if(is_iostat_end(ios))exit
      call tmp%init(n1)
      do i=1,n1
         call tmp%push(a(n2)%pop())
      end do
      do i=1,n1
         call a(n3)%push(tmp%pop())
      end do
      call tmp%clear()
   end do
   write(*,"(*(g0))")(a(i)%top(),i=1,9)
   call a%clear()
end program main
