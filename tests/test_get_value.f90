program test_get_value

  use bmif_2_0, only: BMI_SUCCESS, BMI_FAILURE
  use bmiprmsgroundwater
  use fixtures, only: config_file, status, print_1darray, isReal4EqualReal4, &
      isReal8EqualReal8, print_i_1darray, print_array, isintEqualint, print_d_1darray

  implicit none

  type (bmi_prms_groundwater) :: m
  integer :: retcode

  !test r32 gwres_flow
  retcode = test1()
  if (retcode.ne.BMI_SUCCESS) then
    stop BMI_FAILURE
  end if

  !r64 by nhru gwres_stor_ante.
  retcode = test2()
  if (retcode.ne.BMI_SUCCESS) then
     stop BMI_FAILURE
  end if

    !int by 1 has_gwstor_minarea
  retcode = test3()
  if (retcode.ne.BMI_SUCCESS) then
     stop BMI_FAILURE
  end if

    !test nowtime i(6)
  retcode = test4()
  if (retcode.ne.BMI_SUCCESS) then
     stop BMI_FAILURE
  end if

contains


  ! Test getting r32 gwres_flow.
  function test1() result(status)
    character (len=*), parameter :: &
         var_name = "gwres_flow"
    integer, parameter :: rank = 1
    integer, parameter :: size = 14
    integer, parameter, dimension(rank) :: shape = (/ 14 /)
    real, parameter, dimension(shape(1)) :: &
         expected = (/ 5.2482327E-03, 6.2567892E-04, &
            4.2298837E-03, 4.6067876E-03, 4.1147745E-03, 6.0999682E-03, &
            3.0489832E-03, 6.7496800E-04, 1.1592689E-03, 2.0469553E-03, &
            2.0728302E-03,6.5202604E-04, 1.7155614E-03, 4.4758637E-03 /)
    real :: tval(size)
    integer :: i, status
    double precision :: endtime
    
    status = m%initialize(config_file)
    status = m%get_end_time(endtime)
    status = m%update()
    status = m%get_value(var_name, tval)
    status = m%finalize()

    ! Visual inspection.
    write(*,*) "Test 1"
    
    write(*,*) "get Value"
    call print_1darray(tval, shape)
    
    write(*,*) "Expected"
    call print_1darray(expected, shape)

    status = BMI_SUCCESS
    do i = 1, size
       if (tval(i).ne.expected(i)) then
          status = BMI_FAILURE
       end if
    end do
  end function test1

  ! Test r64 by nhru.
  function test2() result(status)
    character (len=*), parameter :: &
         var_name = "gwres_stor_ante"
    integer, parameter :: rank = 1
    integer, parameter :: size = 14
    integer, parameter, dimension(rank) :: shape = (/ 14 /)
    double precision, parameter, dimension(shape(1)) :: &
         expected = (/ 0.132364004850388, 1.370599959045649e-002, &
            0.110671997070312, 0.113049998879433, 0.119337998330593, 0.162882998585701, &
            6.904400140047073e-002, 1.985199935734272e-002, 3.202399984002113e-002, 4.814099892973900e-002, &
            5.773900076746941e-002, 1.853399910032749e-002, 5.053199827671051e-002, 0.110981002449989 /)
    double precision :: tval(size)
    integer :: i, status
    double precision :: endtime
    
    status = m%initialize(config_file)
    status = m%get_end_time(endtime)
    status = m%update()
    status = m%get_value(var_name, tval)
    status = m%finalize()

    ! Visual inspection.
    write(*,*) "Test 2"
    
    write(*,*) "get Value"
    call print_d_1darray(tval, shape)
    
    write(*,*) "Expected"
    call print_d_1darray(expected, shape)

    status = BMI_SUCCESS
    do i = 1, size
       if (tval(i).ne.expected(i)) then
          status = BMI_FAILURE
       end if
    end do
  end function test2

  ! Test int by 1
  function test3() result(status)
    character (len=*), parameter :: &
         var_name = "has_gwstor_minarea"
    integer, parameter :: rank = 1
    integer, parameter :: size = 1
    integer, parameter, dimension(rank) :: shape = (/1/)
    integer, parameter, dimension(shape(1)) :: &
         expected = (/ 0 /)
    integer :: tval(size)
    integer :: i, status
    double precision :: endtime
    
    status = m%initialize(config_file)
    status = m%get_end_time(endtime)
    status = m%update()
    status = m%get_value(var_name, tval)
    status = m%finalize()

    ! Visual inspection.
    write(*,*) "Test 3"

    write(*,*) "get Value"
    call print_i_1darray(tval, shape)
    
    write(*,*) "Expected"
    call print_i_1darray(expected, shape)

    status = BMI_SUCCESS
    do i = 1, size
       if (tval(i).ne.expected(i)) then
          status = BMI_FAILURE
       end if
    end do
  end function test3

  !test nowtime
  function test4() result(status)
    character (len=*), parameter :: &
         var_name = "nowtime"
    integer, parameter :: rank = 1
    integer, parameter :: size = 6
    integer, parameter, dimension(rank) :: shape = (/ 6 /)
    integer, parameter, dimension(shape(1)) :: &
         expected = (/ 2016, 1, 31, 0, 0, 0 /)
    integer :: tval(size)
    integer :: i, status
    double precision :: endtime
    
    status = m%initialize(config_file)
    status = m%get_end_time(endtime)
    do i = 1,int(endtime)
        status = m%update()
        if(i == endtime) then
            status = m%get_value(var_name, tval)
        endif
    enddo
    !status = m%get_value(var_name, tval)
    status = m%finalize()
  
    ! Visual inspection.
    write(*,*) "Test 4"

    write(*,*) "get Value"
    call print_i_1darray(tval, shape)
    
    write(*,*) "Expected"
    call print_i_1darray(expected, shape)
  
    status = BMI_SUCCESS
    do i = 1, size
       if (tval(i).ne.expected(i)) then
          status = BMI_FAILURE
       end if
    end do
  end function test4
  
  end program test_get_value
