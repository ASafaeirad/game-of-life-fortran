program main
   use ffi
   use kinds_mod
   use argv_mod
   use grid_mod
   use game_of_life_mod

   implicit none

   integer(rk) :: rows, cols, file_grid(100, 100)
   integer(rk) :: generation, generations = 10, speed = 100000
   character(len=256) :: input_path = 'input'

   call parse_cli_args(generations, input_path, speed)
   call read_from_file(input_path, file_grid, rows, cols)
   call initialize_game_state(file_grid, rows, cols)
   call render()

   do generation = 1, generations
      call calculate_next_generation()
      call usleep(speed)
      call render()
   end do
end program main
