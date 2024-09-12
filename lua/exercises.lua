function change(amount)
  if math.type(amount) ~= "integer" then
    error("Amount must be an integer")
  end
  if amount < 0 then
    error("Amount cannot be negative")
  end
  local counts, remaining = {}, amount
  for _, denomination in ipairs({25, 10, 5, 1}) do
    counts[denomination] = remaining // denomination
    remaining = remaining % denomination
  end
  return counts
end



function first_then_lower_case(sequence, predicate)
  for _, element in ipairs(sequence) do
    if predicate(element) and type(element) == "string" then
      return string.lower(element)
    end
  end
  return nil
end




function powers_generator(base, limit)
  return coroutine.create(function()
    local power = 1
    while power <= limit do
      coroutine.yield(power)
      power = power * base
    end
  end)
end




function say(word)
  local result = {}

  local function next_word(input_word)
    if input_word == nil then return table.concat(result, "") end
    if input_word == " " and #result > 0 and result[#result] ~= " " then
      table.insert(result, " ")
    elseif input_word ~= " " then
      if #result > 0 and result[#result] ~= " " then table.insert(result," ") end
      table.insert(result, input_word)
    end
    return next_word
  end

  return next_word(word)
end




function meaningful_line_count(filename)
  local file, err = io.open(filename, "r")
  if not file then 
    error("No such file")
  end

  local count = 0
  for line in file:lines() do
    local counted_line = line:match ("^%s*(.-)%s*$") --learned how to cut down white space (youtube)
    if counted_line ~= "" and #counted_line > 1 and counted_line:sub (1,1) ~= "#" then
      count = count + 1
    end
  end

  file:close()
  return count
end




Quaternion = {}
Quaternion.__index = Quaternion


function Quaternion.new(a, b, c, d)
    local self = setmetatable({}, Quaternion)
    self.a = a
    self.b = b
    self.c = c
    self.d = d
    return self
end


function Quaternion.__add(q1, q2)
    return Quaternion.new(
        q1.a + q2.a,
        q1.b + q2.b,
        q1.c + q2.c,
        q1.d + q2.d
    )
end


function Quaternion.__mul(q1, q2)
    local a1, b1, c1, d1 = q1.a, q1.b, q1.c, q1.d
    local a2, b2, c2, d2 = q2.a, q2.b, q2.c, q2.d

    return Quaternion.new(
        a1 * a2 - b1 * b2 - c1 * c2 - d1 * d2,
        a1 * b2 + b1 * a2 + c1 * d2 - d1 * c2,
        a1 * c2 - b1 * d2 + c1 * a2 + d1 * b2,
        a1 * d2 + b1 * c2 - c1 * b2 + d1 * a2
    )
end


function Quaternion.__eq(q1, q2)
    return q1.a == q2.a and q1.b == q2.b and q1.c == q2.c and q1.d == q2.d
end


function Quaternion:__tostring()
    local terms = {}
    if self.a ~= 0 then table.insert(terms, tostring(self.a)) end
    if self.b ~= 0 then
        local bTerm = self.b == 1 and "i" or self.b == -1 and "-i" or tostring(self.b) .. "i"
        table.insert(terms, bTerm)
    end
    if self.c ~= 0 then
        local cTerm = self.c == 1 and "j" or self.c == -1 and "-j" or tostring(self.c) .. "j"
        table.insert(terms, cTerm)
    end
    if self.d ~= 0 then
        local dTerm = self.d == 1 and "k" or self.d == -1 and "-k" or tostring(self.d) .. "k"
        table.insert(terms, dTerm)
    end
    if #terms == 0 then return "0" end

    local result = terms[1]
    for i = 2, #terms do
        local term = terms[i]
        if term:sub(1, 1) == "-" then
            result = result .. term
        else
            result = result .. "+" .. term
        end
    end
    return result
end


function Quaternion:toJSON()
    return string.format("Quaternion(%s, %s, %s, %s)", self.a, self.b, self.c, self.d)
end


function Quaternion:coefficients()
    return {self.a, self.b, self.c, self.d}
end


function Quaternion:conjugate()
    return Quaternion.new(self.a, -self.b, -self.c, -self.d)
end


function Quaternion:norm()
    return math.sqrt(self.a^2 + self.b^2 + self.c^2 + self.d^2)
end
