program main
   use kinds_mod
   use grid_mod
   use game_of_life

   implicit none

   interface usleep
      subroutine usleep(microseconds) bind(C)
         use iso_c_binding, only: c_int
         integer(c_int), value :: microseconds
      end subroutine usleep
   end interface usleep

   integer(rk) :: rows = 1, cols = 1
   integer(rk) :: file_grid(100, 100)
   integer(rk) :: generation, generations = 10

   integer(rk) :: arg_count, arg_index
   real :: arg_speed
   integer(rk) :: speed = 100000
   character(len=256) :: arg
   character(len=256) :: input_path = 'input'
   integer(rk) :: ios

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
         speed = int(real(speed, kind=kind(arg_speed)) / arg_speed, kind=rk)

       case default
         print '(2A)', 'Unknown argument: ', trim(arg)
         stop 1
      end select

      arg_index = arg_index + 1
   end do

   call read_from_file(input_path, file_grid, rows, cols)
   call initialize_game_state(file_grid, rows, cols)
   call render()
   do generation = 1, generations
      call calculate_next_generation()
      call usleep(speed)
      call render()
   end do
end program main
