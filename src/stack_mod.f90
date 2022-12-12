module stack_mod
   implicit none
   type string
      character(:),allocatable::str
   contains
      final::string_final
   end type string
   type stack
      type(string),allocatable::a(:)
      integer::num,capacity
   contains
      procedure,pass::init    => stack_init
      procedure,pass::push    => stack_push
      procedure,pass::pop     => stack_pop
      procedure,pass::top     => stack_top
      procedure,pass::reverse => stack_reverse
      procedure,pass::clear   => stack_clear
      procedure,pass::stack_set_capacity
   end type stack
   interface optval
      module procedure optval_int
   end interface optval
contains
   integer function optval_int(i,default)result(res)
      integer,optional,intent(in)::i
      integer,intent(in)::default
      if(present(i))then
         res=i
      else
         res=default
      end if
   end function optval_int

   impure elemental subroutine stack_init(this,n)
      class(stack),intent(inout)::this
      integer,optional,intent(in)::n
      this%num=0
      this%capacity=optval(n,10)
      allocate(this%a(this%capacity))
   end subroutine stack_init

   subroutine stack_set_capacity(this)
      class(stack),intent(inout)::this
      integer::capacity
      type(string),allocatable::tmp(:)
      capacity=this%capacity+this%capacity
      allocate(tmp(capacity))
      tmp(1:this%capacity)=this%a
      deallocate(this%a)
      call move_alloc(tmp,this%a)
      this%capacity=capacity
   end subroutine stack_set_capacity

   subroutine stack_push(this,val)
      class(stack),intent(inout)::this
      character(len=*),intent(in)::val
      this%num=this%num+1
      if(this%num>this%capacity) call this%stack_set_capacity()
      this%a(this%num)%str=val
   end subroutine stack_push

   function stack_pop(this) result(val)
      class(stack),intent(inout)::this
      character(len=:),allocatable::val
      val=this%a(this%num)%str
      this%num=this%num-1
   end function stack_pop

   function stack_top(this) result(val)
      class(stack),intent(inout)::this
      character(len=:),allocatable::val
      val=this%a(this%num)%str
   end function stack_top

   elemental subroutine stack_reverse(this)
      class(stack),intent(inout)::this
      this%a(1:this%num)=this%a(this%num:1:-1)
   end subroutine stack_reverse

   elemental subroutine stack_clear(this)
      class(stack),intent(inout)::this
      deallocate(this%a)
   end subroutine stack_clear

   subroutine string_final(this)
      type(string),intent(inout)::this
      deallocate(this%str)
   end subroutine string_final
end module stack_mod

module stack_int_mod
   implicit none
   private
   public::stack_int
   type stack_int
      integer,allocatable::a(:)
      integer::num,capacity
   contains
      procedure,pass::init    => stack_init
      procedure,pass::push    => stack_push
      procedure,pass::pop     => stack_pop
      procedure,pass::top     => stack_top
      procedure,pass::reverse => stack_reverse
      procedure,pass::clear   => stack_clear
      procedure,pass::empty   => stack_empty
      procedure,pass::stack_set_capacity
   end type stack_int
   interface optval
      module procedure optval_int
   end interface optval
contains
   integer function optval_int(i,default)result(res)
      integer,optional,intent(in)::i
      integer,intent(in)::default
      if(present(i))then
         res=i
      else
         res=default
      end if
   end function optval_int

   impure elemental subroutine stack_init(this,n)
      class(stack_int),intent(inout)::this
      integer,optional,intent(in)::n
      this%num=0
      this%capacity=optval(n,10)
      allocate(this%a(this%capacity))
   end subroutine stack_init

   subroutine stack_set_capacity(this)
      class(stack_int),intent(inout)::this
      integer::capacity
      integer,allocatable::tmp(:)
      capacity=this%capacity+this%capacity
      allocate(tmp(capacity))
      tmp(1:this%capacity)=this%a
      deallocate(this%a)
      call move_alloc(tmp,this%a)
      this%capacity=capacity
   end subroutine stack_set_capacity

   subroutine stack_push(this,val)
      class(stack_int),intent(inout)::this
      integer,intent(in)::val
      this%num=this%num+1
      if(this%num>this%capacity) call this%stack_set_capacity()
      this%a(this%num)=val
   end subroutine stack_push

   integer function stack_pop(this) result(val)
      class(stack_int),intent(inout)::this
      val=this%a(this%num)
      this%num=this%num-1
   end function stack_pop

   integer function stack_top(this) result(val)
      class(stack_int),intent(inout)::this
      val=this%a(this%num)
   end function stack_top


   logical function stack_empty(this) result(res)
      class(stack_int),intent(inout)::this
      res=(this%num==0)
   end function stack_empty

   elemental subroutine stack_reverse(this)
      class(stack_int),intent(inout)::this
      this%a(1:this%num)=this%a(this%num:1:-1)
   end subroutine stack_reverse

   elemental subroutine stack_clear(this)
      class(stack_int),intent(inout)::this
      deallocate(this%a)
   end subroutine stack_clear
end module stack_int_mod
