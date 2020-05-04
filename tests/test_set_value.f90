    program test_set_value

    use bmif_2_0, only: BMI_SUCCESS, BMI_FAILURE
    use bmiprmsgroundwater
    use fixtures, only: config_file, status, print_1darray, isReal4EqualReal4, &
        isReal8EqualReal8, print_i_1darray, print_array, isintEqualint, print_d_1darray

    implicit none

    type (bmi_prms_groundwater) :: m
    integer :: retcode

    !test r32 gwsink_coef
    retcode = test1()
    if (retcode.ne.BMI_SUCCESS) then
        stop BMI_FAILURE
    end if

    !test r32 gwres_stor
    retcode = test2()
    if (retcode.ne.BMI_SUCCESS) then
        stop BMI_FAILURE
    end if

    contains

    ! Test setting r32 hru_area.
    function test1() result(code)
    character (len=*), parameter :: &
        var_name = "gwsink_coef"
    integer, parameter :: size = 14
    integer :: dims(1) = size
    real, parameter :: expected(size) = (/ 0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25, &
        0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25 /)
    real :: val(size), val2(size)
    integer :: i, code

    code = m%initialize(config_file)
    code = m%get_value(var_name, val)
    val = 0.25
    code = m%set_value(var_name, val)
    code = m%get_value(var_name, val2)
    code = m%finalize()

    ! Visual inspection.
    write(*,*) "Test 1"
    write(*,*) "get Value"
    call print_1darray(val2, dims)
    write(*,*) "Expected"
    call print_1darray(expected, dims)

    code = BMI_SUCCESS
    do i = 1, size
        if (val2(i).ne.expected(i)) then
            code = BMI_FAILURE
        end if
    end do
    end function test1

    ! Test setting r64 gwres_stor.
    function test2() result(code)
    character (len=*), parameter :: &
        var_name = "gwres_stor"
    integer, parameter :: size = 14
    integer :: dims(1) = size
    double precision, parameter :: expected(size) = (/ &
        0.0025d0, 0.0025d0, 0.0025d0, 0.0025d0, &
        0.0025d0, 0.0025d0, 0.0025d0, 0.0025d0, &
        0.0025d0, 0.0025d0, 0.0025d0, 0.0025d0, &
        0.0025d0, 0.0025d0 /)
    double precision :: val(size), val2(size)
    integer :: i, code

    code = m%initialize(config_file)
    code = m%get_value(var_name, val)
    val = 0.0025d0
    code = m%set_value(var_name, val)
    code = m%get_value(var_name, val2)
    code = m%finalize()

    ! Visual inspection.
    write(*,*) "Test 2"
    write(*,*) "get Value"
    call print_d_1darray(val2, dims)
    write(*,*) "Expected"
    call print_d_1darray(expected, dims)

    code = BMI_SUCCESS
    do i = 1, size
        if (val2(i).ne.expected(i)) then
            code = BMI_FAILURE
        end if
    end do
    end function test2

    end program test_set_value
