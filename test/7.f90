!https://github.com/aradi/AoC-2022
program main
   implicit none
   integer,allocatable::num(:),dir(:)
   integer::filesize,ios
   character(len=30)::str
   character(len=20)::filename
   integer::tmp
   num=[0]
   dir=[1]
   open(10,file="data/7.txt")
   do
      read(10,"(A)",iostat=ios)str
      if(is_iostat_end(ios))exit
      if(str(1:4)=="$ cd")then
         ! meet $ cd
         select case(trim(str(6:)))
         ! prev dir
         case('..');dir=dir(1:size(dir)-1)
         case('/');dir=[dir(1:1)]
         case default
            num = [num,0]
            dir= [dir,size(num)]
         end select 
      elseif(str(1:4)/="dir".and.str(1:4)/="$ ls")then
         read(str,*)filesize,filename
         num(dir)=num(dir)+filesize
      end if
   end do
   close(10)
   write(*,*)sum(pack(num,num<=100000))
   tmp=30000000-(70000000-num(1))
   write(*,*)minval(pack(num,num>=tmp))
end program main
