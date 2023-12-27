package co.com.sura.mapbox;

import co.com.sura.mapbox.entity.DireccionResponse;
import co.com.sura.mapbox.entity.GeoUbicacion;
import co.com.sura.mapbox.entity.Route;
import co.com.sura.mapbox.repository.map.services.MapboxServicesImpl;
import com.mapbox.api.directions.v5.MapboxDirections;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.web.client.RestTemplate;
import org.springframework.web.util.UriTemplateHandler;
import reactor.core.publisher.Mono;
import reactor.test.StepVerifier;

import java.util.Collections;

@ExtendWith(MockitoExtension.class)
 class MapboxServicesTest {

    private static final int DURACION_MINIMA = 900;
    @Mock
    private  RestTemplate restTemplateMock;
    @Mock
    private  MapboxDirections mapboxDirectionsMock;

    @InjectMocks
    private MapboxServicesImpl mapboxServicesimpl;


    @Test
    void calcularTiempoViaje(){
        UriTemplateHandler uriTemplateHandlerMock = Mockito.mock(UriTemplateHandler.class);
        int duracion = 1800;
        GeoUbicacion geoInit = GeoUbicacion.builder()
                .longitud(11.2)
                .latitud(-7.2)
                .build();
        GeoUbicacion geoDestino = GeoUbicacion.builder()
                .longitud(11.4)
                .latitud(-7.4)
                .build();

        Mockito.when(restTemplateMock.getUriTemplateHandler())
                .thenReturn(uriTemplateHandlerMock);

        var lon1 = geoInit.getLongitud();
        var lat1 = geoInit.getLatitud();
        var lon2 = geoDestino.getLongitud();
        var lat2 = geoDestino.getLatitud();
        var uriExpanded = uriTemplateHandlerMock.expand("{lon1},{lat1};{lon2},{lat2}", lon1, lat1, lon2, lat2);
        Mockito.when(restTemplateMock.getForObject(uriExpanded,DireccionResponse.class))
                .thenReturn(DireccionResponse.builder()
                        .routes(Collections.singletonList(Route.builder()
                                .durationTypical(duracion)
                                .build()))
                        .build());

        Mono<Integer> response = mapboxServicesimpl.calcularTiempoViaje(geoInit,geoDestino);

        StepVerifier.create(response)
                .expectNext(duracion)
                .verifyComplete();
    }
    @Test
    void calcularTiempoViajeDuracionMinima(){
        UriTemplateHandler uriTemplateHandlerMock = Mockito.mock(UriTemplateHandler.class);

        GeoUbicacion geoInit = GeoUbicacion.builder()
                .longitud(11.2)
                .latitud(-7.2)
                .build();
        GeoUbicacion geoDestino = GeoUbicacion.builder()
                .longitud(11.4)
                .latitud(-7.4)
                .build();

        Mockito.when(restTemplateMock.getUriTemplateHandler())
                .thenReturn(uriTemplateHandlerMock);

        var lon1 = geoInit.getLongitud();
        var lat1 = geoInit.getLatitud();
        var lon2 = geoDestino.getLongitud();
        var lat2 = geoDestino.getLatitud();
        var uriExpanded = uriTemplateHandlerMock.expand("{lon1},{lat1};{lon2},{lat2}", lon1, lat1, lon2, lat2);
        Mockito.when(restTemplateMock.getForObject(uriExpanded,DireccionResponse.class))
                .thenReturn(DireccionResponse.builder()
                        .routes(Collections.singletonList(Route.builder()
                                .durationTypical(600)
                                .build()))
                        .build());

        Mono<Integer> response = mapboxServicesimpl.calcularTiempoViaje(geoInit,geoDestino);

        StepVerifier.create(response)
                .expectNext(DURACION_MINIMA)
                .verifyComplete();
    }


}
