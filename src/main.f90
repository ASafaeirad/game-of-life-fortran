program main
   use ffi
   use kinds_mod
   use argv_mod
   use grid_mod
   use game_of_life_mod
   use terminal_mod

   implicit none

   integer(rk) :: rows, cols, file_grid(100, 100)
   integer(rk) :: generation = 1, generations = 10, timeout = 100000
   integer(rk) :: keyboard_action
   integer(rk), parameter :: poll_interval = 10000
   character(len=256) :: input_path = 'input'

   call parse_cli_args(generations, input_path, timeout)
   call read_from_file(input_path, file_grid, rows, cols)
   call initialize_game_state(file_grid, rows, cols)
   call enable_raw_input()
   call render()

   do
      call poll_keyboard_during_timeout(timeout, poll_interval, keyboard_action)
      if (keyboard_action == keyboard_action_reset) then
         call reset_game()
         generation = 0
      else if (keyboard_action == keyboard_action_quit) then
         call disable_raw_input()
         print '(A)', 'Exiting...'
         stop
      end if

      if (generation < generations) then
         call calculate_next_generation()
         generation = generation + 1
         call render()
      end if
   end do
end program main
