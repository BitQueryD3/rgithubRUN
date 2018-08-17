
# LB
ctx = interactive.login("XXX", "YYY", scopes=c("repo"))

print(ctx$user$login)
print(ctx$user$id)
