module argv_mod
   use kinds_mod
   implicit none
   private

   public :: parse_cli_args
contains

   subroutine parse_cli_args(generations, input_path, speed)
      integer(rk), intent(inout) :: generations
      character(len=*), intent(inout) :: input_path
      integer(rk), intent(inout) :: speed

      integer(rk) :: arg_count, arg_index
      integer(rk) :: ios
      real :: arg_speed
      character(len=256) :: arg
      integer(rk), parameter :: base_speed = 100000_rk

      arg_count = command_argument_count()
      arg_index = 1

      do while (arg_index <= arg_count)
         call get_command_argument(arg_index, arg)

         select case (trim(arg))
          case ('-g')
            if (arg_index >= arg_count) then
               print '(A)', 'Missing value for -g'
               stop 1
            end if

            arg_index = arg_index + 1
            call get_command_argument(arg_index, arg)
            read(arg, *, iostat=ios) generations
            if (ios /= 0) then
               print '(A)', 'Invalid value for -g'
               stop 1
            end if

          case ('-f')
            if (arg_index >= arg_count) then
               print '(A)', 'Missing value for -f'
               stop 1
            end if

            arg_index = arg_index + 1
            call get_command_argument(arg_index, input_path)
          case ('-s')
            if (arg_index >= arg_count) then
               print '(A)', 'Missing value for -s'
               stop 1
            end if

            arg_index = arg_index + 1
            call get_command_argument(arg_index, arg)
            read(arg, *, iostat=ios) arg_speed
            if (ios /= 0) then
               print '(A)', 'Invalid value for -s'
               stop 1
            end if
            if (arg_speed <= 0.0) then
               print '(A)', 'Value for -s must be greater than 0'
               stop 1
            end if
            speed = int(real(base_speed, kind=kind(arg_speed)) / arg_speed, kind=rk)
          case default
            print '(2A)', 'Unknown argument: ', trim(arg)
            stop 1
         end select

         arg_index = arg_index + 1
      end do
   end subroutine parse_cli_args

end module argv_mod
