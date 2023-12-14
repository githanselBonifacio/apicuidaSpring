package co.com.sura.mapbox.gateway;


import co.com.sura.mapbox.entity.GeoUbicacion;
import reactor.core.publisher.Mono;

public interface MapboxServiceRepository {
    Mono<Integer> calcularTiempoViaje(GeoUbicacion puntoInicio, GeoUbicacion puntoDestino);
    Mono<Integer> calcularTiempoViajeMapboxSDK(GeoUbicacion puntoInicio, GeoUbicacion puntoDestino);
}
