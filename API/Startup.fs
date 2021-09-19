module Startup

open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Hosting
open Microsoft.Extensions.Configuration
open Microsoft.Extensions.DependencyInjection
open Microsoft.Extensions.Hosting
open Microsoft.OpenApi.Models
open Engine


type Startup(configuration: IConfiguration) =
    member _.Configuration = configuration

    // This method gets called by the runtime. Use this method to add services to the container.
    member _.ConfigureServices(services: IServiceCollection) =
        let info = OpenApiInfo()
        info.Title <- "BME (Betting Matching Engine) API"
        info.Version <- "v1"

        services
            .AddSwaggerGen(fun config -> config.SwaggerDoc("v1", info))
            .AddSingleton(Engine())
            .AddControllers() |> ignore

    // This method gets called by the runtime. Use this method to configure the HTTP request pipeline.
    member _.Configure(app: IApplicationBuilder, env: IWebHostEnvironment) =
        if (env.IsDevelopment()) then
            app.UseDeveloperExceptionPage() |> ignore
        app.UseHttpsRedirection()
           .UseRouting()
           .UseAuthorization()
           .UseSwagger()
           .UseSwaggerUI(fun config -> config.SwaggerEndpoint("/swagger/v1/swagger.json", "BME (Betting Matching Engine) API"))
           .UseEndpoints(fun endpoints ->
                endpoints.MapControllers() |> ignore
            ) |> ignore
