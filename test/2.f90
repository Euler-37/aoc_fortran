program main
   use iso_fortran_env
   implicit none
   character::a,x
   integer::ia,ix
   integer::ios
   integer::score
   integer::map(3,3),win(3)
   map(1,:)=[3,0,6]
   map(2,:)=[6,3,0]
   map(3,:)=[0,6,3]
   win=[0,3,6]
   open(10,file="data/2.txt")
   score=0
   do
      read(10,*,iostat=ios)a,x
      if(is_iostat_end(ios))exit
      ia=ichar(a)-ichar('A')+1
      ix=ichar(x)-ichar('X')+1
      score=score+ix+map(ix,ia)
   end do
   write(*,*)score
   rewind(10)
   map(1,:)=[3,1,2]
   map(2,:)=[1,2,3]
   map(3,:)=[2,3,1]
   score=0
   do
      read(10,*,iostat=ios)a,x
      if(is_iostat_end(ios))exit
      ia=ichar(a)-ichar('A')+1
      ix=ichar(x)-ichar('X')+1
      score=score+map(ix,ia)+win(ix)
   end do
   write(*,*)score
   close(10)
end program main
