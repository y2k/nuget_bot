FROM mcr.microsoft.com/dotnet/sdk:5.0.202-alpine3.12-amd64

ARG MYGET_TOKEN
ARG GITHUB_TOKEN

WORKDIR /app
COPY . /app

RUN dotnet test
RUN dotnet publish -c Release -r linux-x64 --self-contained false

# sdk нужен что бы собирать проекты внутри контейнера
FROM mcr.microsoft.com/dotnet/sdk:5.0.202-alpine3.12-amd64

WORKDIR /app
COPY --from=0 /app/app/bin/Release/netcoreapp3.1/linux-x64/publish .

ENTRYPOINT ["dotnet", "app.dll"]
