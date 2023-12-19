package co.com.sura.web.farmacia;


import co.com.sura.agenda.entity.PacienteTratamientoCita;
import co.com.sura.farmacia.FarmaciaUseCase;
import co.com.sura.farmacia.gateway.FarmaciaRepository;
import co.com.sura.genericos.Response;
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
import reactor.core.publisher.Mono;

import java.time.LocalDate;
import java.util.ArrayList;
import java.util.List;

@ContextConfiguration(classes = FarmaciaController.class)
@WebFluxTest(controllers = FarmaciaController.class,excludeAutoConfiguration = {ReactiveSecurityAutoConfiguration.class})
@ExtendWith(SpringExtension.class)
@ExtendWith(MockitoExtension.class)
@Import(FarmaciaUseCase.class)
 class FarmaciaControllerTest {

    @MockBean
    private FarmaciaRepository farmaciaRepositoryMock;

    @Autowired
    private WebTestClient webClient;

    @Test
    void getProfesionalesbyTurnoRegional(){
        List<PacienteTratamientoCita> pacienteTratamiento = new ArrayList<>();
        pacienteTratamiento.add(PacienteTratamientoCita.builder().numeroIdentificacion("989898").build());

        Response<List<PacienteTratamientoCita>> respuestaEsperada = Response.<List<PacienteTratamientoCita>>builder()
                .result(pacienteTratamiento)
                .build();

        Mockito.when(farmaciaRepositoryMock.consultarAllPacienteWithMedicamentosToFarmacia())
                .thenReturn(Flux.fromIterable(pacienteTratamiento));


        Response<List<PacienteTratamientoCita>> response = webClient.get()
                .uri("/farmacia/tratamientosFarmacia")
                .exchange()
                .expectStatus().isOk()
                .expectBody(new ParameterizedTypeReference<Response<List<PacienteTratamientoCita>>>(){})
                .returnResult().getResponseBody();

        Assertions.assertNotNull(response);
        Assertions.assertEquals(respuestaEsperada.getResult(), response.getResult());
    }

    @Test
    void consultarMedicamentosToFarmaciaWithFilter(){
        LocalDate fechaTurno = LocalDate.now();
        String idRegional = "427";
        Integer idHorarioTurno = 1;
        List<PacienteTratamientoCita> pacienteTratamiento = new ArrayList<>();
        pacienteTratamiento.add(PacienteTratamientoCita.builder().numeroIdentificacion("989898").build());

        Response<List<PacienteTratamientoCita>> respuestaEsperada = Response.<List<PacienteTratamientoCita>>builder()
                .result(pacienteTratamiento)
                .build();

        Mockito.when(farmaciaRepositoryMock.consultarAllPacienteWithMedicamentosToFarmaciaByFilter(
                fechaTurno,idHorarioTurno,idRegional
                ))
                .thenReturn(Flux.fromIterable(pacienteTratamiento));

        UriComponentsBuilder builderUrl = UriComponentsBuilder.fromPath("/farmacia/tratamientosFarmaciaWithFilter")
                .queryParam("fechaTurno", fechaTurno)
                .queryParam("idHorarioTurno", idHorarioTurno)
                .queryParam("idRegional", idRegional);

        Response<List<PacienteTratamientoCita>> response = webClient.get()
                .uri(builderUrl.build().toUri())
                .exchange()
                .expectStatus().isOk()
                .expectBody(new ParameterizedTypeReference<Response<List<PacienteTratamientoCita>>>(){})
                .returnResult().getResponseBody();

        Assertions.assertNotNull(response);
        Assertions.assertEquals(respuestaEsperada.getResult(), response.getResult());
    }

    @Test
    void notificarMedicamentosToFarmacia(){
        String identificacionPaciente = "9898998";
        List<PacienteTratamientoCita> pacientesTratamiento = new ArrayList<>();
        pacientesTratamiento.add(PacienteTratamientoCita.builder()
                .numeroIdentificacion(identificacionPaciente).build());

        Mockito.when(farmaciaRepositoryMock.notificarMedicamentosToFarmacia(pacientesTratamiento))
                .thenReturn(Mono.just(Boolean.TRUE));

        Response<Boolean> respuesta = webClient.post()
                .uri("/farmacia/notificarFarmacia")
                .bodyValue(pacientesTratamiento)
                .exchange()
                .expectBody(new ParameterizedTypeReference<Response<Boolean>>(){})
                .returnResult()
                .getResponseBody();

        Assertions.assertNotNull(respuesta);
        Assertions.assertTrue(respuesta.getResult());
    }
}
