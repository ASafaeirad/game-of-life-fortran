program main
   use ffi
   use kinds_mod
   use argv_mod
   use grid_mod
   use game_of_life_mod

   implicit none

   integer(rk) :: rows, cols, file_grid(100, 100)
   integer(rk) :: generation = 1, generations = 10, timeout = 100000
   integer(rk) :: remaining_sleep, sleep_chunk
   integer(rk), parameter :: poll_interval = 10000
   character(len=256) :: input_path = 'input'
   character(len=1) :: key
   logical :: has_key

   call parse_cli_args(generations, input_path, timeout)
   call read_from_file(input_path, file_grid, rows, cols)
   call initialize_game_state(file_grid, rows, cols)
   call enable_raw_input()
   call render()

   do while(generation < generations)
      call calculate_next_generation()

      remaining_sleep = timeout

      do while (remaining_sleep > 0)
         sleep_chunk = min(poll_interval, remaining_sleep)
         call usleep(sleep_chunk)
         remaining_sleep = remaining_sleep - sleep_chunk

         call read_key_nonblocking(key, has_key)
         if (has_key) then
            if (key == 'r' .or. key == 'R') then
               call reset_game()
               generation = 0
               exit
            end if
            if (key == 'q' .or. key == 'Q') then
               call disable_raw_input()
               print '(A)', 'Exiting...'
               stop
            end if
            if (key == 'p' .or. key == 'P') then
               print '(A)', 'Paused. Press "p" to resume.'
               do
                  call read_key_nonblocking(key, has_key)
                  if (has_key .and. (key == 'p' .or. key == 'P')) then
                     print '(A)', 'Resuming...'
                     exit
                  end if
                  call usleep(poll_interval)
               end do
            end if
         end if
      end do

      generation = generation + 1
      call render()
   end do

   call disable_raw_input()
end program main
