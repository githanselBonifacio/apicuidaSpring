package co.com.sura.services.mapbox;


import reactor.core.publisher.Mono;

public interface MapboxServiceRepository {
    Mono<Integer> calcularTiempoViaje(GeoUbicacion puntoInicio, GeoUbicacion puntoDestino);
    Mono<Integer> calcularTiempoViajeMapboxSDK(GeoUbicacion puntoInicio, GeoUbicacion puntoDestino);
}
