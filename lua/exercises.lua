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

-- Write your first then lower case function here
function first_then_lower_case(sequence, predicate)
  for _, element in ipairs(sequence) do
    if predicate(element) and type(element) == "string" then
      return string.lower(element)
    end
  end
  return nil
end



-- Write your powers generator here
function powers_generator(base, limit)
  return coroutine.create(function()
    local power = 1
    while power <= limit do
      coroutine.yield(power)
      power = power * base
    end
  end)
end



-- Write your say function here
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



-- Write your line count function here

-- Write your Quaternion table here
