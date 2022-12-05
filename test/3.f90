program main
   use iso_fortran_env
   implicit none
   character(len=80)::s
   integer::ios,sums
   integer::i,ia,j,maps(54,3)
   open(10,file="data/3.txt")
   sums=0
   do
      read(10,"(A)",iostat=ios)s
      if(is_iostat_end(ios))exit
      j=len_trim(s)/2
      i=scan(s(:j),s(j+1:))
      select case(s(i:i))
      case('a':'z');ia=ichar(s(i:i))-ichar('a')+1
      case('A':'Z');ia=ichar(s(i:i))-ichar('A')+27
      end select
      sums=sums+ia
   end do
   write(*,*)sums
   sums=0
   rewind(10)
   outer:do
      maps=0
      do j=1,3
         read(10,"(A)",iostat=ios)s
         if(is_iostat_end(ios))exit outer
         do i=1,len_trim(s)
            select case(s(i:i))
            case('a':'z');ia=ichar(s(i:i))-ichar('a')+1
            case('A':'Z');ia=ichar(s(i:i))-ichar('A')+27
            end select
            maps(ia,j)=1
         end do
      end do
      sums=sums+findloc(sum(maps,dim=2),3,dim=1)
   end do outer
   write(*,*)sums
   close(10)
end program main
