package co.com.sura.services.mapbox;


import reactor.core.publisher.Mono;

public interface MapboxService {
    Mono<Integer> calcularTiempoViaje(GeoUbicacion puntoInicio, GeoUbicacion puntoDestino);
    Mono<Integer> calcularTiempoViajeMapboxSDK(GeoUbicacion puntoInicio, GeoUbicacion puntoDestino);
}
