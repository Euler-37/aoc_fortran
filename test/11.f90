module monkey_mod
   use fortran_vector_i64
   implicit none
   type monkey
      type(vector_int)::iterms
      character(2)::oper
      integer(8)::num
      integer(8)::move(3)
   contains
      procedure,pass::monkey_print
      procedure,pass::calculate
   end type monkey

contains
   impure elemental subroutine monkey_print(this)
      class(monkey),intent(in)::this
      write(*,"(A)",advance="no")"iterm:["
      write(*,"(*(g0,:,1x))",advance="no")this%iterms%x_(size(this%iterms))
      write(*,"(A)",advance="no")"]"
      write(*,*)
      write(*,"(*(g0,:,1x))")"operator:",this%oper,this%num
      write(*,"(*(g0,:,1x))")"move:",this%move
   end subroutine monkey_print


   integer(8) function calculate(this,x)result(res)
      class(monkey),intent(in)::this
      integer(8),intent(in)::x
      select case(this%oper)
      case("+");res=x+this%num
      case("*");res=x*this%num
      case("**")
         res=x*x
         if(res<0)write(*,*)res
      end select
   end function calculate
end module monkey_mod

program main
   use aoc_fortran
   use iso_fortran_env
   use monkey_mod
   implicit none
   integer(8),parameter::mm=7
   type(monkey)::a(0:mm)
   type(monkey)::b(0:mm)
   character(len=80)::str
   integer(8)::i,j,idx,n,ios,lcms
   integer(8),allocatable::x(:)
   open(10,file="data/11.txt")
   do i=0,mm
      read(10,"(A)")str
      read(10,"(A)")str
      ! read iterm
      idx=index(str,":")
      n=getcolnum(str(idx+1:))
      allocate(x(n))
      call a(i)%iterms%init(n)
      read(str(idx+1:),*)x
      call a(i)%iterms%append(x)
      ! read oper
      read(10,"(A)")str
      idx=index(str,"old")+2
      if(index(str(idx+1:),"old")/=0)then
         a(i)%oper="**"
         a(i)%num=2
      else
         a(i)%oper=str(idx+2:idx+2)
         a(i)%num=tonum(str(idx+3:))
      end if
      read(10,"(A)")str
      idx=index(str,"by")+1
      a(i)%move(1)=tonum(str(idx+1:))
      read(10,"(A)")str
      idx=index(str,"monkey")+5
      a(i)%move(2)=tonum(str(idx+1:))
      read(10,"(A)")str
      idx=index(str,"monkey")+5
      a(i)%move(3)=tonum(str(idx+1:))
      read(10,*,iostat=ios)
      deallocate(x)
   end do
   b=a
   allocate(x(0:mm))
   x=a%move(1)
   lcms=1
   do i=0,mm
      lcms=lcm(x(i),lcms)
   end do
   !lcms=reduce(x,lcm) ! ifort
   x=0
   do j=1,20
      do i=0,mm
         x(i)=x(i)+size(a(i)%iterms)
         call monkey_update(a,i,0_8)
      end do
   end do
   block
      type(vector_int)::tmp
      call tmp%init(capacity_=mm+1)
      call tmp%append(x)
      call tmp%sort()
      write(*,*)tmp%get(mm+1)*tmp%get(mm)
   end block
   x=0
   do j=1,10000
      do i=0,mm
         x(i)=x(i)+size(b(i)%iterms)
         call monkey_update(b,i,lcms)
      end do
   end do
   block
      type(vector_int)::tmp
      call tmp%init(capacity_=mm+1)
      call tmp%append(x)
      call tmp%sort()
      write(*,*)tmp%get(mm+1)*tmp%get(mm)
   end block
contains
   subroutine monkey_update(this,i,lcms)
      type(monkey),intent(inout)::this(0:)
      integer(8),intent(in)::i
      integer(8)::level,j,lcms
      do while(size(this(i)%iterms)/=0)
         level=this(i)%iterms%pop()
         level=this(i)%calculate(level)
         if(lcms==0)then
            level=level/3
         else
            level=mod(level,lcms)
         end if
         if(mod(level,this(i)%move(1))==0)then
            j=this(i)%move(2)
         else
            j=this(i)%move(3)
         end if
         call this(j)%iterms%append(level)
      end do
   end subroutine monkey_update
end program main
