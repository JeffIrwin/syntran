clear
clc
xdel(winsid())
me = "plot-syntran-wave-eq.sce"
chdir(get_absolute_file_path(me))
mprintf('\n')

fd=mopen('wave-output.txt','r')

// Skip banner header
txt = ''
while part(txt, 1:13) ~= " Interpreting"
    txt = mgetl(fd, 1)
end

i = 0
while 1==1
    i = i + 1
    
    mprintf('%d\n', i)
    
    txt = mgetl(fd, 1)
    if meof(fd) then
        break
    end
    
//    if (i ~= 900)
//        continue
//    end
    
    // Extract substring by trimming brackets []
    sub = part(txt, 3: length(txt) - 1)
    
    // Split by commas and read numbers
    u1 = strtod(tokens(sub, ','))
    nx = sqrt(length(u1))
    ny = nx
    u = matrix(u1, nx, ny)

    clf()
    
//    plot(u, 'LineWidth', 3)
//    xlabel('Position')
//    ylabel('Wave amplitude')
//    a = gca()
//    a.data_bounds = [1, -1; length(u), 1]
//    xs2png(gcf(), sprintf("frames/wave_%d.png", i))
    
     
    surf(u)
    gcf().color_map = jetcolormap(100);
    //set(gcf(), "axes_size", [800 350], "rotation_style","multiple");
    //gca().rotation_angles = [40 -60];
//    gca().rotation_angles = [3.75  -116.];
    gca().data_bounds = [1 1 -1; nx ny 1]
//    colorbar(-1, 1)

    //xs2png(gcf(), sprintf("frames/wave_%d.png", i))
    xs2svg(gcf(), sprintf("frames/wave_%d.svg", i))
//    xs2eps(gcf(), sprintf("frames/wave_%d.eps", i))
    
//    break
    
end

mclose(fd)

exit

