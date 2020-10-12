FROM mcr.microsoft.com/dotnet/core/sdk:3.1.402-alpine3.12

ARG MYGET_TOKEN
ARG GITHUB_TOKEN

WORKDIR /app
COPY . /app

RUN dotnet test
RUN dotnet publish -c Release -r linux-x64 --self-contained false

# sdk нужен что бы собирать проекты внутри контейнера
FROM mcr.microsoft.com/dotnet/core/sdk:3.1.402-alpine3.12

WORKDIR /app
COPY --from=0 /app/app/bin/Release/netcoreapp3.1/linux-x64/publish .

ENTRYPOINT ["dotnet", "app.dll"]
