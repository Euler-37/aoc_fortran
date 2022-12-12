program main
   use dijkstra_mod
   implicit none
   integer,parameter::n=41,m=117
   type(link_mat)::a(m*n)
   character::str(m,n)
   integer::i,j,k,posi,posj,posk,i0(2),path
   integer::move(2,4)
   open(10,file="data/12.txt")
   do i=1,n
      read(10,"(*(A1))")str(:,i)
   end do
   move(:,1)=[-1,0]
   move(:,2)=[ 1,0]
   move(:,3)=[0,-1]
   move(:,4)=[ 0,1]
   i0=findloc(str, 'S')
   str(i0(1),i0(2))='a'
   posi=map_idx(i0,n)
   i0=findloc(str, 'E')
   str(i0(1),i0(2))='z'
   posj=map_idx(i0,n)
   do i=1,m
      do j=1,n
         posk=map_idx([i,j],n)
         do k=1,4
            i0=[i,j]+move(:,k)
            if(all(i0>=1.and.i0<=[m,n]))then
               if(ichar(str(i,j))+1>=ichar(str(i0(1),i0(2)))) call a(posk)%append([map_idx(i0,n),1])
            end if
         end do
         call a(posk)%cut()
      end do
   end do
   write(*,*) dijkstra(a,posi,posj)
   path=huge(1)
   do i=1,m
      do j=1,n
         if(str(i,j)=="a")then
            path=min(path,dijkstra(a,map_idx([i,j],n),posj))
         end if
      end do
   end do
   write(*,*)path
contains
   function map_idx(i,n)
      integer,intent(in)::i(2)
      integer,intent(in)::n
      integer::map_idx
      map_idx=(i(1)-1)*n+i(2)
   end function map_idx

end program main
