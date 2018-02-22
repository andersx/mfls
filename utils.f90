! MIT License
!
! Copyright (c) 2018 Anders Steen Christensen
!
! Permission is hereby granted, free of charge, to any person obtaining a copy
! of this software and associated documentation files (the "Software"), to deal
! in the Software without restriction, including without limitation the rights
! to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
! copies of the Software, and to permit persons to whom the Software is
! furnished to do so, subject to the following conditions:
!
! The above copyright notice and this permission notice shall be included in all
! copies or substantial portions of the Software.
!
! THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
! IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
! FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
! AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
! LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
! OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
! SOFTWARE.

module mfls

    implicit none

contains


function argsort(vector) result(sorting)

    double precision, intent(in), dimension(:) :: vector
    
    double precision, allocatable, dimension(:) :: scratch
    integer, allocatable, dimension(:) :: sorting

    integer :: i, k
    
    integer :: length

    double precision, parameter :: huge_double = huge(0.0d0)
    
    length = size(vector, dim=1)

    allocate(scratch(length))
    allocate(sorting(length))

    sorting(:) = 0
    scratch(:) = vector(:)
   
    do i = 1, length
        k = minloc(scratch(:), dim=1)
        sorting(i) = k
        scratch(k) = huge_double

    enddo

    deallocate(scratch)

end function argsort 

end module mfls
