module aoc_fortran
  implicit none
contains
   subroutine replace(str,b,c)
      character(len=*),intent(inout)::str
      character,intent(in)::b,c
      integer::i
      do i=1,len_trim(str)
         if(str(i:i)==b) str(i:i)=c
      end do
   end subroutine replace
end module aoc_fortran
