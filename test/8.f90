program main
   implicit none
   integer,parameter::n=99
   integer::a(n,n),signs(n,n)
   integer::i,j
   open(10,file="data/8.txt")
   do i=1,n
      read(10,"(*(I1))")a(:,i)
   end do
   close(10)
   signs=0
   signs(1,:)=1
   signs(:,1)=1
   signs(n,:)=1
   signs(:,n)=1
   do i=2,n-1
      do j=2,n-1
         signs(i,j)=can_see(a,i,j)
      end do
   end do
   write(*,*)sum(signs)
   signs=0
   do i=2,n-1
      do j=2,n-1
         signs(i,j)=score(a,i,j)
      end do
   end do
   write(*,*)maxval(signs)
contains

   integer function score(a,i,j)result(res)
      integer,intent(in)::a(:,:)
      integer,intent(in)::i,j
      integer::x(4)
      x(1)=    findloc(a(i,j+1:)>=a(i,j),.true.,dim=1)
      x(2)=j-1-findloc(a(i,:j-1)>=a(i,j),.true.,dim=1,back=.true.)
      x(3)=i-1-findloc(a(:i-1,j)>=a(i,j),.true.,dim=1,back=.true.)
      x(4)=    findloc(a(i+1:,j)>=a(i,j),.true.,dim=1)
      x=merge([n-j,x(2),x(3),n-i],x,x==0)
      res=product(x)
   end function score

   integer function can_see(a,i,j)result(res)
      integer,intent(in)::a(:,:)
      integer,intent(in)::i,j
      res=0
      if(all(a(:i-1,j)<a(i,j)).or. &
         all(a(i,:j-1)<a(i,j)).or. &
         all(a(i,j+1:)<a(i,j)).or. &
         all(a(i+1:,j)<a(i,j)))then
         res=1
      end if
   end function can_see
end program main
