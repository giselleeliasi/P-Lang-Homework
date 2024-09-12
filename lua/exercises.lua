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

-- Write your line count function here

-- Write your Quaternion table here
