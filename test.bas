func is_prime(n)
    if n < 2 then return 0 end
    if n == 2 then return 1 end
    if n % 2 == 0 then return 0 end
    i = 3
    while i * i <= n do
        if n % i == 0 then return 0 end
        i = i + 1
    end
    return 1
end

i = 2
while i < 30 do
    if is_prime(i) then
        print i
    end
    i = i + 1
end
