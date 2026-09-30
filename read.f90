program read_dr_file
    use, intrinsic :: iso_fortran_env, only : iostat_end
    implicit none    
    integer :: nprnti, nprntf, ntemps, nlvl
    integer :: i, j, k, ios, transition_idx
    integer :: idx_i, idx_f
    character(len=256) :: line
    logical :: eof 
    real, dimension(:,:,:), allocatable :: dr_rates
    real, dimension(:,:),   allocatable :: dr_rates_total
    real, dimension(:),     allocatable :: tempgrid
    
    ntemps = 19 
    
    open(unit=10, file='adf09_3231', status='old', action='read')
    
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
    
    do
        read(10, '(A)', iostat=ios) line
        if (ios /= 0) stop "Error: NLVL header not found."
        
        idx_i = index(line, 'NLVL=')
        if (idx_i > 0 ) then
            read(line(idx_i+5:idx_i+5+3), *) nlvl
            exit
        end if
    end do
    print*, nlvl

    allocate(dr_rates      (nprnti, nlvl, ntemps))
    allocate(dr_rates_total(nprnti, ntemps)        )
    allocate(tempgrid(ntemps)        )

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
        do j = 1, nlvl
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
    eof = .false. 

    do while (.not. eof) 
        read(10, '(A8)', iostat=ios) line
        if (ios == iostat_end) eof = .true. 
        if (line .eq. '    T(K)') then 
            !print*,line
            read(10, '(A8)', iostat=ios) line
            do k = 1, ntemps 
                read(10, '(2X,ES8.2, 3X, 100(ES8.2,3X) )' ) tempgrid(k),(dr_rates_total(i,k),i=1,nprnti)
            end do
            exit 
        end if 
    end do 
    
    !print*, dr_rates_total(1,:) / sum( dr_rates(1,:,:),dim=1)
    !print*, shape(sum( dr_rates(1,:,:),dim=2))
    close(10)
    !print*, sum( dr_rates(1,1:25,8)) / sum( dr_rates(1,:,8))
end program read_dr_file