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

   pure function tostring(value)result(res)
      integer(4),intent(in)      :: value
      character(len=:),allocatable :: res
      integer(4),parameter       :: buffer_len=range(value)+2
      character(len=buffer_len)    :: buffer
      integer(4)                 :: pos,n
      character(len=1),parameter   :: numbers(0:9)=["0","1","2","3","4","5","6","7","8","9"]
      if(value == 0)then
         res = numbers(0)
         return
      end if
      n=abs(value)
      buffer=""
      pos=buffer_len+1
      do while(n > 0)
         pos=pos-1
         buffer(pos:pos)=numbers(mod(n,10))
         n=n/10
      end do
      if(value < 0)then
         pos=pos-1
         buffer(pos:pos)="-"
      end if
      res=buffer(pos:)
   end function tostring

   !> description : convert string to integer(4)
   !>
   !> example     : tonum('123') => 123
   !> example     : tonum('-123') => -123
   function tonum(str)result(res)
      character(len=*),intent(in)::str
      integer(4)::res
      character(len=len_trim(adjustl(str)))::str_trim
      integer(4)::i,lens,sign1
      str_trim=trim(adjustl(str))
      lens=len(str_trim)
      res=0
      sign1=merge(-1,1,str_trim(1:1)=="-")
      do i=lens,merge(2,1,sign1==-1),-1
         res=res+(ichar(str_trim(i:i))-ichar("0"))*10**(lens-i)
      end do
      res=res*sign1
   end function tonum

   !> description : get row number from string
   !> same as fortran split tokens
   !>
   !> example     : getrownum("abcdefg") => 1
   !> example     : getrownum("abcdefg abc") => 2
   function getcolnum(str) result(res)
      character(len=*),intent(in)::str
      integer(kind=4)::res
      integer(kind=4)::i,j,ios
      character(len=1)::c
      res=1
      do i=1,len_trim(str)
         read(str,*,iostat=ios)(c,j=1,i)
         if(ios/=0)then
            res=i-1
            exit
         end if
      end do
   end function getcolnum

   !> description : check if character is digit
   !>
   !> example     : isdigit('1') => true
   !> example     : isdigit('a') => false
   function isdigit(s)result(res)
      character(len=1),intent(in)::s
      logical(kind=4)::res
      res=s >="0".and. s<="9"
   end function isdigit

   !> description : get time
   !> 
   !> example     : tic = clock()
   !> example     : spend = clock() - tic
   function clock()result(res)
      real(kind=8)::res
      integer(kind=8)::tic
      integer(kind=8)::rate
      call system_clock(tic,count_rate=rate)
      res=real(tic,8)/rate
   end function clock

    integer(8) function string_hashcode(str)result(h)
        character(len=*),intent(in)::str
        integer::i
        integer(8)::g
        integer(8),parameter::PP=int(z"F0000000",8)
        do i=1,len(str)
            h=shiftl(h,4)+ichar(str(i:i))
            g=iand(h,pp)
            if(g/=0)h=ieor(h,shiftl(g,24))
            h=iand(h,not(g))
        end do
    end function string_hashcode

    elemental integer(8) function lcm(m,n)result(r)
       integer(8),intent(in)::m,n
       r=m*n/gcd(m,n)
    end function lcm

    elemental integer(8) function gcd(m,n)result(r)
       integer(8),intent(in)::m,n
       integer(8)::a,b
       a=max(m,n)
       b=min(m,n)
       do
          r=mod(a,b)
          if(r==0)exit
          a=b
          b=r
       end do
       r=b
    end function gcd
        ! This function generates the next permutation of an array
    ! cpp <algorithm> :: next_permutation
    logical function next_permutation(a)result(found)
        integer,intent(inout)::a(:)
        integer::i,j
        found=.false.
        associate(n=>size(a))
            do i=n-1,1,-1
               if(a(i)<a(i+1))then
                  found=.true.
                  exit
               end if
            end do
            if(.not.found) then
                a=a(n::-1)
                return
            end if
            do j=n,i+1,-1
               if(a(i)<a(j))exit
            end do
            a([i,j])=a([j,i])
            a(i+1:n)=a(n:i+1:-1)
        end associate
    end function next_permutation
 end module aoc_fortran
