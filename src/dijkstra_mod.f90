module dijkstra_mod
   use stack_int_mod
   implicit none
   private
   public::link_mat,size,dijkstra
   type link_mat
      integer,allocatable::near(:)
      integer,allocatable::dis(:)
      integer::num=-1
      integer::capacity
   contains
      procedure,pass,private::link_mat_resize
      procedure,pass::append=>link_mat_append
      procedure,pass::cut=>link_mat_cut
   end type link_mat
   
   interface size
      module procedure link_mat_size
   end interface size
contains

   function link_mat_size(this)result(n)
      !! size of vector
      type(link_mat),intent(in)::this
      integer::n
      n=this%num
   end function link_mat_size

   subroutine link_mat_resize(this)
      class(link_mat),intent(inout)::this
      integer,allocatable::tmp(:)
      this%capacity=this%capacity+this%capacity
      allocate(tmp(this%capacity))
      tmp(1:this%num)=this%near
      deallocate(this%near)
      call move_alloc(tmp,this%near)
      allocate(tmp(this%capacity))
      tmp(1:this%num)=this%dis
      deallocate(this%dis)
      call move_alloc(tmp,this%dis)
   end subroutine link_mat_resize

   subroutine link_mat_append(this,a)
      class(link_mat),intent(inout)::this
      integer,intent(in)::a(2)
      if(this%num==-1)then
         this%num=0
         this%capacity=10
         allocate(this%near(this%capacity))
         allocate(this%dis(this%capacity))
      end if
      if(this%num+1>this%capacity)then
         call this%link_mat_resize()
      end if
      this%num=this%num+1
      this%near(this%num)=a(1)
      this%dis(this%num) =a(2)
   end subroutine link_mat_append

   subroutine link_mat_cut(this)
      class(link_mat),intent(inout)::this
      integer,allocatable::tmp(:)
      this%capacity=this%num
      allocate(tmp(this%capacity))
      tmp(1:this%num)=this%near(1:this%num)
      deallocate(this%near)
      call move_alloc(tmp,this%near)
      allocate(tmp(this%capacity))
      tmp(1:this%num)=this%dis(1:this%num)
      deallocate(this%dis)
      call move_alloc(tmp,this%dis)
   end subroutine link_mat_cut


   integer function dijkstra(mat,posi,posj,path)result(res)
      type(link_mat),intent(in)::mat(:)
      integer,intent(in)::posi,posj
      integer,allocatable::node(:)
      integer,allocatable::distance(:)
      logical,allocatable::is_add(:)
      type(stack_int),optional::path
      integer::m,i,dis
      integer::pos,val
      m=size(mat)
      allocate(distance(m))
      allocate(is_add(m))
      if(present(path)) allocate(node(m),source=0)
      distance=huge(1)
      is_add=.false.
      !! add init node
      is_add(posi)=.true.
      do i=1,size(mat(posi))
         if(present(path)) node(mat(posi)%near(i))=posi
         distance(mat(posi)%near(i))=mat(posi)%dis(i)
      end do
      !do while(any(is_add==0))
      do while(.not.is_add(posj))
         !! find min distance
         pos=minloc(distance,dim=1,mask=.not.is_add)
         !! add node
         is_add(pos)=.true.
         val=distance(pos)
         if(val==huge(1))then
            res=huge(1)
            exit
         end if
         !! update distance
         do i=1,size(mat(pos))
            associate(posk=>mat(pos)%near(i))
               if(.not.is_add(posk))then
                  dis=val+mat(pos)%dis(i)
                  if(distance(posk)>dis)then
                     if(present(path))node(posk)=pos
                     distance(posk)=dis
                  end if
               end if
            end associate
         end do
      end do
      if(present(path))then
         call path%init()
         i=posj
         do
            call path%push(i)
            if(node(i)==posi)exit
            i=node(i)
         end do
         call path%push(posi)
         deallocate(node)
      end if
      res=distance(posj)
      deallocate(distance)
      deallocate(is_add)
   end function dijkstra

end module dijkstra_mod
