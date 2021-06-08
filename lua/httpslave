local default_ip = "localhost"
local default_port = "7112"

print("These parameters must match those")
print("given to the AdaBots program")

print("")
print("Please enter an IP Address")
print("Just press enter for the default:")
print(default_ip)
print("")

local ip = read()
if ip == "" then
  ip = default_ip
end

print("")
print("Please enter an IP port")
print("Just press enter for the default:")
print(default_port)
print("")

local port = read()
if port == "" then
  port = default_port
end

local server = "http://" .. ip .. ":" .. port .. "/"

print("Okay, listening on: " .. server)
print("")

while true do

  local request = nil
  repeat
    request = http.get(server)
    if not request then
      os.sleep(1)
    end
  until request

  local command = request.readAll()
  local functor = loadstring("return(" .. command .. ")")

  local result = nil
  if functor then
    result = tostring(functor())
  else
    result = "error: invalid lua expression"
  end
  print(command .. " returned " .. result)

  http.get(server .. "return_value/" .. result)

  request.close()

end
