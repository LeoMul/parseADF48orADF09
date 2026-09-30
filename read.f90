program read_dr_file
    implicit none
    integer :: nprnti, nprntf, ntemps
    integer :: i, j, k, ios, transition_idx
    integer :: idx_i, idx_f
    character(len=256) :: line
    real, dimension(:,:,:), allocatable :: dr_rates
    
    ntemps = 19 
    
    open(unit=10, file='adf09', status='old', action='read')
    
    ! Extract array bounds from header
    do
        read(10, '(A)', iostat=ios) line
        if (ios /= 0) stop "Error: NPRNTI/NPRNTF header not found."
        
        idx_i = index(line, 'NPRNTI=')
        idx_f = index(line, 'NPRNTF=')
        
        if (idx_i > 0 .and. idx_f > 0) then
            read(line(idx_i+7:idx_f-1), *) nprnti
            read(line(idx_f+7:), *) nprntf
            exit
        end if
    end do
    
    allocate(dr_rates(nprnti, nprntf, ntemps))
    dr_rates = 0.0  ! Initialize so any skipped transitions default to 0.0
    
    ! Loop sequentially through PRTI blocks
    do i = 1, nprnti
        
        ! Search forward for PRTI marker
        do
            read(10, '(A)', iostat=ios) line
            if (ios /= 0) stop "Error: Reached EOF searching for PRTI marker."
            if (index(line, 'PRTI') > 0) exit
        end do
        
        ! Locate the header divider line ('----')
        do
            read(10, '(A)', iostat=ios) line
            if (ios /= 0) stop "Error: Reached EOF searching for header divider."
            if (index(line, '----') > 0) exit
        end do
        
        ! Read transitions, handling both internal skips and early block terminations
        do j = 1, nprntf
            
            ! 1. Peek ahead to the next non-blank line
            do
                read(10, '(A)', iostat=ios) line
                if (ios /= 0) exit
                if (len_trim(line) > 0) exit
            end do
            
            ! 2. Try extracting just the transition index from the string
            read(line, *, iostat=ios) transition_idx
            
            ! If it fails to read a number, we hit a text marker (ILREP/INREP/PRTI).
            ! This means the LAST expected transition(s) are missing.
            if (ios /= 0) then
                backspace(10)  ! Rewind the text marker so the outer loop can find it
                exit           ! Escape 'j' loop. Remaining transitions stay 0.0
            end if
            
            ! 3. Check for internally skipped transitions
            if (transition_idx /= j) then
                backspace(10)  ! Rewind this data line
                cycle          ! Move to j+1, leaving dr_rates(i, j, :) as 0.0
            end if
            
            ! 4. Match confirmed! Rewind the peeked line and do the full 2-line read.
            backspace(10)
            read(10, *, iostat=ios) transition_idx, (dr_rates(i, j, k), k=1, ntemps)
            
            if (ios /= 0) then
                print *, "Read error/EOF at PRTI block", i, "Transition", j
                stop
            end if
            
        end do
        
    end do
    
    close(10)
    !print*, dr_rates(1,9800,8)
    !print '(19ES10.2)', sum( dr_rates(2,:,:),dim=1)
    !print '(19ES10.2)', sum(dr_rates(58,:,8))
    
    print*, sum( dr_rates(1,1:25,8)) / sum( dr_rates(1,:,8))

    !print*, sum( dr_rates(1:10,1:100,8)) / sum( dr_rates(1:10,:,8))

    !print *, "Successfully read data, handling all skipped/missing edge cases."
    
end program read_dr_file