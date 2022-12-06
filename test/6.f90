program main
   use iso_fortran_env
   implicit none
   character::s4(4),b,s14(14)
   integer::k,ios
   open(10,file="data/6.txt")
   read(10,"(4A1)",advance="no")s4
   k=4
   if(is_not_same(s4,4))then
      write(*,*)k
   else
      do
         read(10,"(A1)",advance="no",iostat=ios)b
         if(is_iostat_end(ios))exit
         k=k+1
         s4=[s4(2:),b]
         if(is_not_same(s4,4))then
            write(*,*)k
            exit
         end if
      end do
   end if
   rewind(10)
   read(10,"(14A1)",advance="no")s14
   k=14
   if(is_not_same(s14,14))then
      write(*,*)k
   else
      do
         read(10,"(A1)",advance="no",iostat=ios)b
         if(is_iostat_end(ios))exit
         k=k+1
         s14=[s14(2:),b]
         if(is_not_same(s14,14))then
            write(*,*)k
            exit
         end if
      end do
   end if
   close(10)
contains
   logical function is_not_same(s,n)result(res)
      integer,intent(in)::n
      character,intent(in)::s(n)
      integer::i,j
      res=.true.
      do i=1,n-1
         do j=i+1,n
            if(s(i)==s(j))then
               res=.false.
               return
            end if
         end do
      end do
   end function is_not_same
end program main
