package co.com.sura.mapbox.repository.map.services;

import co.com.sura.services.mapbox.MapboxService;
import co.com.sura.services.mapbox.GeoUbicacion;

import co.com.sura.response.mapbox.DireccionResponse;
import com.mapbox.api.directions.v5.MapboxDirections;
import com.mapbox.geojson.Point;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.web.client.RestTemplate;
import reactor.core.publisher.Mono;

@Service
public class MapboxServicesImpl implements MapboxService {
    private static final int DURACION_MINIMA = 900;
    private final RestTemplate restTemplate;
    private final MapboxDirections mapboxDirections;

    @Autowired
    public MapboxServicesImpl(RestTemplate restTemplate, MapboxDirections mapboxDirections) {
        this.restTemplate = restTemplate;
        this.mapboxDirections = mapboxDirections;
    }


    @Override
     public Mono<Integer> calcularTiempoViaje(GeoUbicacion puntoInicio, GeoUbicacion puntoDestino) {
        try {
            var uriTemplate = restTemplate.getUriTemplateHandler();
            var lon1 = puntoInicio.getLongitud();
            var lat1 = puntoInicio.getLatitud();
            var lon2 = puntoDestino.getLongitud();
            var lat2 = puntoDestino.getLatitud();
            var uriExpanded = uriTemplate.expand("{lon1},{lat1};{lon2},{lat2}", lon1, lat1, lon2, lat2);
            var response = restTemplate.getForObject(uriExpanded, DireccionResponse.class);

            assert response != null : "respuesta mapbox duracion viaje";
            var duracion = (int) Math.round(response.getRoutes().get(0).getDurationTypical());
            if (duracion < DURACION_MINIMA) {
                return Mono.just(DURACION_MINIMA);
            } else {
                return Mono.just((duracion));
            }
        }catch (Exception e){
            return Mono.just(0);
        }
    }

    @Override
    public Mono<Integer> calcularTiempoViajeMapboxSDK(GeoUbicacion puntoInicio, GeoUbicacion puntoDestino) {
        try {
            var directions = mapboxDirections.toBuilder()
                    .origin(Point.fromLngLat(puntoInicio.getLongitud(), puntoInicio.getLatitud()))
                    .destination(Point.fromLngLat(puntoDestino.getLongitud(), puntoDestino.getLatitud()))
                    .build();
            var response = directions.executeCall();

            assert response != null : "respuesta mapbox duracion viaje";

            var duracion = (int) Math.round(response.body().routes().get(0).durationTypical());
            if (duracion < DURACION_MINIMA) {
                return Mono.just(DURACION_MINIMA);
            } else {
                return Mono.just((duracion));
            }
        } catch (Exception e) {
            return Mono.just(0);
        }
    }

}
