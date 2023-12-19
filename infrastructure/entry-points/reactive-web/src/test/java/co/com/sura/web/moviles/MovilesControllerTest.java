package co.com.sura.web.moviles;

import co.com.sura.genericos.Response;
import co.com.sura.moviles.MovilesUseCase;
import co.com.sura.moviles.entity.Desplazamiento;
import co.com.sura.moviles.gateway.MovilRepository;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.autoconfigure.security.reactive.ReactiveSecurityAutoConfiguration;
import org.springframework.boot.test.autoconfigure.web.reactive.WebFluxTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.context.annotation.Import;
import org.springframework.core.ParameterizedTypeReference;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit.jupiter.SpringExtension;
import org.springframework.test.web.reactive.server.WebTestClient;
import org.springframework.web.util.UriComponentsBuilder;
import reactor.core.publisher.Flux;

import java.time.LocalDate;
import java.util.ArrayList;
import java.util.List;

@ContextConfiguration(classes = MovilesController.class)
@WebFluxTest(controllers = MovilesController.class,excludeAutoConfiguration = {ReactiveSecurityAutoConfiguration.class})
@ExtendWith(SpringExtension.class)
@ExtendWith(MockitoExtension.class)
@Import(MovilesUseCase.class)
 class MovilesControllerTest {

    @MockBean
    private MovilRepository movilRepositoryMock;

    @Autowired
    private WebTestClient webClient;

    @Test
    void getDesplazamientoByTurnoRegional(){
        LocalDate fechaTurno = LocalDate.now();
        String idRegional = "427";
        Integer idHorarioTurno = 1;
        List<Desplazamiento> desplazamientos = new ArrayList<>();
        desplazamientos.add(Desplazamiento.builder().build());

        Response<List<Desplazamiento>> respuestaEsperada = Response.<List<Desplazamiento>>builder()
                .result(desplazamientos)
                .build();

        Mockito.when(movilRepositoryMock.consultarDesplazamientoRegional(fechaTurno,idHorarioTurno,idRegional))
                .thenReturn(Flux.fromIterable(desplazamientos));

        UriComponentsBuilder builderUrl = UriComponentsBuilder.fromPath("/moviles/desplazamientoVisita")
                .queryParam("fechaTurno", fechaTurno)
                .queryParam("idHorarioTurno", idHorarioTurno)
                .queryParam("idRegional", idRegional);

        Response<List<Desplazamiento>> response = webClient.get()
                .uri(builderUrl.build().toUri())
                .exchange()
                .expectStatus().isOk()
                .expectBody(new ParameterizedTypeReference<Response<List<Desplazamiento>>>(){})
                .returnResult().getResponseBody();

        Assertions.assertNotNull(response);
        Assertions.assertEquals(respuestaEsperada.getResult(), response.getResult());
    }
}
