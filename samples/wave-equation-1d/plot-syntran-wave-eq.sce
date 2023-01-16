clear
clc
xdel(winsid())
me = "plot-syntran-wave-eq.sce"
chdir(get_absolute_file_path(me))
mprintf('\n')

//fd=mopen('samples/wave-equation-1d/wave-output.txt','r')
fd=mopen('wave-output.txt','r')

// Skip banner header
txt = ''
//for i = 1: 9
while part(txt, 1:13) ~= " Interpreting"
    txt = mgetl(fd, 1)
end

i = 0
while 1==1
    i = i + 1
    
    txt = mgetl(fd, 1)
    if meof(fd) then
        break
    end
    
    // Extract substring by trimming brackets []
    sub = part(txt, 3: length(txt) - 1)
    
    // Split by commas and read numbers
    u = strtod(tokens(sub, ','))
    
    clf()
    plot(u, 'LineWidth', 3)
    xlabel('Position')
    ylabel('Wave amplitude')
    a = gca()
    a.data_bounds = [1, -1; length(u), 1]
    xs2png(gcf(), sprintf("frames/wave_%d.png", i))
    
end

mclose(fd)

exit

