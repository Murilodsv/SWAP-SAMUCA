subroutine get_co2_samuca(CO2ppm, co2year, mayrs, year, co2)

    Implicit none

    integer, intent(in) ::year
    integer, intent(in) :: mayrs
    real(8), intent(in) :: CO2ppm(mayrs)
    integer, intent(in) :: co2year(mayrs)
    real, intent(out) :: co2

    integer :: CO2year_idx, i, min_year
    save

    min_year = co2year(1)
    do i = 2, size(co2year)
        if (CO2year(i) .gt. 0) min_year = min(min_year, co2year(i))
    enddo

    CO2year_idx = 1 + year - min_year

    if (CO2year_idx .lt. 1) then
        print *, 'Could not find CO2 data for year=',year,' in Atmospheric.co2 file'
        print *, 'CO2 level assumed as CO2ppm(1)=',CO2ppm(1)
        co2 = CO2ppm(1)
     elseif (CO2year_idx .gt. (maxval(CO2year) - min_year+1)) then
        print *, 'Could not find CO2 data for year=',year,' in Atmospheric.co2 file'
        print *, 'CO2 level assumed as CO2ppm(size(CO2ppm))=',CO2ppm(size(CO2ppm))
        co2 = CO2ppm(size(CO2ppm))
     else
        co2 = CO2ppm(CO2year_idx)
     end if

    return

end subroutine get_co2_samuca