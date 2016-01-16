10.times do
  puts "Hello"
end

hash = {
  who: "I.B. Banker",
  where: "Bank",
  what: "phishing with friends"
}

hash.each do |key, val|
  3.times do
    puts "#{key} => #{val}"
  end
end

local = "hello"
hash.each do |key, value, ; local|
  local = "good bye"
  puts "#{key} => #{value} - #{local}"
  local = "hi"
end
