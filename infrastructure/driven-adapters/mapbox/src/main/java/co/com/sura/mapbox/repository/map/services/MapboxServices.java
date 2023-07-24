package co.com.sura.mapbox.repository.map.services;

import co.com.sura.services.mapbox.MapboxService;
import co.com.sura.services.mapbox.GeoUbicacion;

import co.com.sura.response.mapbox.DireccionResponse;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.web.client.RestTemplate;
import reactor.core.publisher.Mono;

@Service
public class MapboxServices implements MapboxService {
    private static final int DURACION_MINIMA = 900;
    private final RestTemplate restTemplate;

    @Autowired
    public MapboxServices(RestTemplate restTemplate) {
        this.restTemplate = restTemplate;
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

}
