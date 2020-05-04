    program test_get_value_ptr

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
    real, pointer :: tptr(:)
    integer :: i, status

    status = m%initialize(config_file)
    status = m%update()
    status = m%get_value_ptr(var_name, tptr)
    
    ! Visual inspection.
    write(*,*) "Test 1"
    
    write(*,*) "Get Value Ptr"
    call print_1darray(tptr, shape)
    
    write(*,*) "Expected"
    call print_1darray(expected, shape)
    
    status = BMI_SUCCESS
    do i = 1, shape(1)
        if (tptr(i).ne.expected(i)) then
            status = BMI_FAILURE
            exit
        end if
    end do

    status = m%finalize()

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
    double precision, pointer :: tptr(:)

    integer :: i, status
    
    status = m%initialize(config_file)
    status = m%update()
    status = m%get_value_ptr(var_name, tptr)
    
    ! Visual inspection.
    write(*,*) "Test 2"
    
    write(*,*) "Get Value Ptr"
    call print_d_1darray(tptr, shape)
    
    write(*,*) "Expected"
    call print_d_1darray(expected, shape)

    status = BMI_SUCCESS
    do i = 1, size
       if (tptr(i).ne.expected(i)) then
          status = BMI_FAILURE
       end if
    end do
    status = m%finalize()
  end function test2

end program test_get_value_ptr
