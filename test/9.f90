program main
   use iso_fortran_env
   implicit none
   write(*,*)solution(2)
   write(*,*)solution(10)
contains
   subroutine move(a,b)
      integer,intent(inout)::a(2),b(2)
      associate(x=>a-b)
         if(abs(x(1)*x(2))>1)then
            b=b+x/abs(x)
         elseif(abs(x(1))>1)then
            b(1)=b(1)+x(1)/abs(x(1))
         elseif(abs(x(2))>1)then
            b(2)=b(2)+x(2)/abs(x(2))
         end if
      end associate
   end subroutine move

   integer function solution(lens)result(num)
      use aoc_fortran
      integer,intent(in)::lens
      integer::rope(2,lens)
      character::d
      integer::step,ios,i,j
      character(len=32)::set(10000),vis
      open(10,file="data/9.txt",action="read")
      num=0
      rope=0
      do
         read(10,*,iostat=ios)d,step
         if(is_iostat_end(ios))exit
         do i=1,step
            rope(:,1)=rope(:,1)+rules(d)
            do j=2,lens
               call move(rope(:,j-1),rope(:,j))
            end do
            vis=tostring(rope(1,lens))//" "//tostring(rope(2,lens))
            if(all(set(1:num)/=vis))then
               num=num+1
               set(num)=vis
            end if
         end do
      end do
      close(10)
   end function solution

   function rules(x)result(h)
      character,intent(in)::x
      integer::h(2)
      select case(x)
      case("U"); h=[ 1, 0]
      case("D"); h=[-1, 0]
      case("R"); h=[ 0, 1]
      case("L"); h=[ 0,-1]
      end select
   end function rules
end program main
