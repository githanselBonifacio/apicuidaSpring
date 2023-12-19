package co.com.sura.web.personal;

import co.com.sura.genericos.Response;
import co.com.sura.genericos.ResultadoActualizacionTurno;
import co.com.sura.moviles.entity.Movil;
import co.com.sura.personal.PersonalUseCase;
import co.com.sura.personal.entity.*;
import co.com.sura.personal.gateway.PersonalCrudRepository;
import co.com.sura.personal.gateway.SecuenciasHorarioRepository;
import co.com.sura.remision.dto.EliminarTurnoProfesionalRequest;
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

@ContextConfiguration(classes = PersonalController.class)
@WebFluxTest(controllers = PersonalController.class,excludeAutoConfiguration = {ReactiveSecurityAutoConfiguration.class})
@ExtendWith(SpringExtension.class)
@ExtendWith(MockitoExtension.class)
@Import(PersonalUseCase.class)
 class PersonalControllerTest {

    private final LocalDate fechaTurno = LocalDate.now();
    private final String idRegional = "427";
    @MockBean
    private PersonalCrudRepository personalCrudRepositoryMock;

    @MockBean
    private SecuenciasHorarioRepository secuenciasHorarioRepositoryMock;
    @Autowired
    private WebTestClient webClient;

    @Test
    void getProfesionales(){
        List<Profesional> profesionales = new ArrayList<>();
        profesionales.add(Profesional.builder().numeroIdentificacion("989898").build());

        Response<List<Profesional>> respuestaEsperada = Response.<List<Profesional>>builder()
                .result(profesionales)
                .build();

        Mockito.when(personalCrudRepositoryMock.consultarProfesionales())
                .thenReturn(Flux.fromIterable(profesionales));

        Response<List<Profesional>> response = webClient.get()
                .uri("/personal/profesionales")
                .exchange()
                .expectStatus().isOk()
                .expectBody(new ParameterizedTypeReference<Response<List<Profesional>>>(){})
                .returnResult().getResponseBody();

        Assertions.assertNotNull(response);
        Assertions.assertEquals(respuestaEsperada.getResult(), response.getResult());

    }

    @Test
    void getProfesionalesByRegional(){
        List<Profesional> profesionales = new ArrayList<>();
        profesionales.add(Profesional.builder().numeroIdentificacion("989898").build());

        Response<List<Profesional>> respuestaEsperada = Response.<List<Profesional>>builder()
                .result(profesionales)
                .build();

        Mockito.when(personalCrudRepositoryMock.consultarProfesionalesByRegional(idRegional))
                .thenReturn(Flux.fromIterable(profesionales));

        Response<List<Profesional>> response = webClient.get()
                .uri("/personal/profesionales/"+idRegional)
                .exchange()
                .expectStatus().isOk()
                .expectBody(new ParameterizedTypeReference<Response<List<Profesional>>>(){})
                .returnResult().getResponseBody();

        Assertions.assertNotNull(response);
        Assertions.assertEquals(respuestaEsperada.getResult(), response.getResult());

    }

    @Test
    void consultarProfesionalesTurnos(){
        List<ProfesionalWithTurno> profesionales = new ArrayList<>();
        profesionales.add(ProfesionalWithTurno.builder().numeroIdentificacion("989898").build());

        Response<List<ProfesionalWithTurno>> respuestaEsperada = Response.<List<ProfesionalWithTurno>>builder()
                .result(profesionales)
                .build();

        Mockito.when(secuenciasHorarioRepositoryMock.consultarHorariosProfesionales(fechaTurno.toString(),idRegional))
                .thenReturn(Flux.fromIterable(profesionales));

        UriComponentsBuilder builderUrl = UriComponentsBuilder.fromPath("/personal/horarioTurno")
                .queryParam("fechaTurno", fechaTurno)
                .queryParam("idRegional", idRegional);

        Response<List<Profesional>> response = webClient.get()
                .uri(builderUrl.build().toUri())
                .exchange()
                .expectStatus().isOk()
                .expectBody(new ParameterizedTypeReference<Response<List<Profesional>>>(){})
                .returnResult().getResponseBody();

        Assertions.assertNotNull(response);
        Assertions.assertEquals(respuestaEsperada.getResult(), response.getResult());

    }
    @Test
    void actualizarTurnoProfesional(){
        List<TurnoProfesional> turnosProfesionales = new ArrayList<>();
        turnosProfesionales.add(TurnoProfesional.builder().idProfesional("98594").build());



        Mockito.when(secuenciasHorarioRepositoryMock.actualizarHorarioTurnoProfesionales(turnosProfesionales))
                .thenReturn(Mono.just(Boolean.TRUE));

        Response<Boolean> response = webClient.put()
                .uri("/personal/actualizarTurnoProfesional")
                .bodyValue(turnosProfesionales)
                .exchange()
                .expectBody(new ParameterizedTypeReference<Response<Boolean>>(){})
                .returnResult()
                .getResponseBody();

        Assertions.assertNotNull(response);
        Assertions.assertTrue(response.getResult());
    }

    @Test
    void crearProfesional(){
        Profesional profesional = Profesional.builder().numeroIdentificacion("98989").build();

        Response<Profesional> respuestaEsperada = Response.<Profesional>builder()
                .result(profesional)
                .build();

        Mockito.when(personalCrudRepositoryMock.crearProfesional(profesional))
                .thenReturn(Mono.just(profesional));

        Response<Profesional> response = webClient.post()
                .uri("/personal/crearProfesional")
                .bodyValue(profesional)
                .exchange()
                .expectBody(new ParameterizedTypeReference<Response<Profesional>>(){})
                .returnResult()
                .getResponseBody();

        Assertions.assertNotNull(response);
        Assertions.assertEquals(respuestaEsperada.getResult(), response.getResult());
    }
    @Test
    void actualizarProfesional(){
        Profesional profesional = Profesional.builder().numeroIdentificacion("98989").build();

        Response<Profesional> respuestaEsperada = Response.<Profesional>builder()
                .result(profesional)
                .build();

        Mockito.when(personalCrudRepositoryMock.actualizarProfesional(profesional))
                .thenReturn(Mono.just(profesional));

        Response<Profesional> response = webClient.put()
                .uri("/personal/actualizarProfesional")
                .bodyValue(profesional)
                .exchange()
                .expectBody(new ParameterizedTypeReference<Response<Profesional>>(){})
                .returnResult()
                .getResponseBody();

        Assertions.assertNotNull(response);
        Assertions.assertEquals(respuestaEsperada.getResult(), response.getResult());
    }

    @Test
    void getConductores(){
        List<Conductor> conductor = new ArrayList<>();
        conductor.add(Conductor.builder().numeroIdentificacion("989898").build());

        Response<List<Conductor>> respuestaEsperada = Response.<List<Conductor>>builder()
                .result(conductor)
                .build();

        Mockito.when(personalCrudRepositoryMock.consultarConductores())
                .thenReturn(Flux.fromIterable(conductor));

        Response<List<Conductor>> response = webClient.get()
                .uri("/personal/conductores")
                .exchange()
                .expectStatus().isOk()
                .expectBody(new ParameterizedTypeReference<Response<List<Conductor>>>(){})
                .returnResult().getResponseBody();

        Assertions.assertNotNull(response);
        Assertions.assertEquals(respuestaEsperada.getResult(), response.getResult());

    }

    @Test
    void crearConductor(){
        Conductor conductor = Conductor.builder().numeroIdentificacion("98989").build();

        Response<Conductor> respuestaEsperada = Response.<Conductor>builder()
                .result(conductor)
                .build();

        Mockito.when(personalCrudRepositoryMock.crearConductor(conductor))
                .thenReturn(Mono.just(conductor));

        Response<Conductor> response = webClient.post()
                .uri("/personal/crearConductor")
                .bodyValue(conductor)
                .exchange()
                .expectBody(new ParameterizedTypeReference<Response<Conductor>>(){})
                .returnResult()
                .getResponseBody();

        Assertions.assertNotNull(response);
        Assertions.assertEquals(respuestaEsperada.getResult(), response.getResult());
    }
    @Test
    void actualizarConductor(){
        Conductor conductor = Conductor.builder().numeroIdentificacion("98989").build();

        Response<Conductor> respuestaEsperada = Response.<Conductor>builder()
                .result(conductor)
                .build();

        Mockito.when(personalCrudRepositoryMock.actualizarConductor(conductor))
                .thenReturn(Mono.just(conductor));

        Response<Conductor> response = webClient.put()
                .uri("/personal/actualizarConductor")
                .bodyValue(conductor)
                .exchange()
                .expectBody(new ParameterizedTypeReference<Response<Conductor>>(){})
                .returnResult()
                .getResponseBody();

        Assertions.assertNotNull(response);
        Assertions.assertEquals(respuestaEsperada.getResult(), response.getResult());
    }

    @Test
    void consultarMoviles(){
        List<Movil> moviles = new ArrayList<>();
        moviles.add(Movil.builder().marca("marca").build());

        Response<List<Movil>> respuestaEsperada = Response.<List<Movil>>builder()
                .result(moviles)
                .build();

        Mockito.when(personalCrudRepositoryMock.consultarMoviles())
                .thenReturn(Flux.fromIterable(moviles));

        Response<List<Movil>> response = webClient.get()
                .uri("/personal/moviles")
                .exchange()
                .expectStatus().isOk()
                .expectBody(new ParameterizedTypeReference<Response<List<Movil>>>(){})
                .returnResult().getResponseBody();

        Assertions.assertNotNull(response);
        Assertions.assertEquals(respuestaEsperada.getResult(), response.getResult());

    }

    @Test
    void crearMovil(){
        Movil movil = Movil.builder().marca("marca").build();

        Response<Movil> respuestaEsperada = Response.<Movil>builder()
                .result(movil)
                .build();

        Mockito.when(personalCrudRepositoryMock.crearMovil(movil))
                .thenReturn(Mono.just(movil));

        Response<Movil> response = webClient.post()
                .uri("/personal/crearMovil")
                .bodyValue(movil)
                .exchange()
                .expectBody(new ParameterizedTypeReference<Response<Movil>>(){})
                .returnResult()
                .getResponseBody();

        Assertions.assertNotNull(response);
        Assertions.assertEquals(respuestaEsperada.getResult(), response.getResult());
    }
    @Test
    void actualizarMovil(){
        Movil movil = Movil.builder().marca("marca").build();

        Response<Movil> respuestaEsperada = Response.<Movil>builder()
                .result(movil)
                .build();

        Mockito.when(personalCrudRepositoryMock.actualizarMovil(movil))
                .thenReturn(Mono.just(movil));

        Response<Movil> response = webClient.put()
                .uri("/personal/actualizarMovil")
                .bodyValue(movil)
                .exchange()
                .expectBody(new ParameterizedTypeReference<Response<Movil>>(){})
                .returnResult()
                .getResponseBody();

        Assertions.assertNotNull(response);
        Assertions.assertEquals(respuestaEsperada.getResult(), response.getResult());
    }
    @Test
    void consultarMovilesSinConductor(){
        List<Movil> moviles = new ArrayList<>();
        moviles.add(Movil.builder().marca("marca").build());

        Response<List<Movil>> respuestaEsperada = Response.<List<Movil>>builder()
                .result(moviles)
                .build();

        Mockito.when(personalCrudRepositoryMock.consultarMovilesSinConductor())
                .thenReturn(Flux.fromIterable(moviles));

        Response<List<Movil>> response = webClient.get()
                .uri("/personal/movilesSinConductor")
                .exchange()
                .expectStatus().isOk()
                .expectBody(new ParameterizedTypeReference<Response<List<Movil>>>(){})
                .returnResult().getResponseBody();

        Assertions.assertNotNull(response);
        Assertions.assertEquals(respuestaEsperada.getResult(), response.getResult());

    }
    @Test
    void consultarMovilesByIdRegional(){
        List<Movil> moviles = new ArrayList<>();
        moviles.add(Movil.builder().marca("marca").build());

        Response<List<Movil>> respuestaEsperada = Response.<List<Movil>>builder()
                .result(moviles)
                .build();

        Mockito.when(personalCrudRepositoryMock.consultarMovilesByIdRegional(idRegional))
                .thenReturn(Flux.fromIterable(moviles));

        Response<List<Movil>> response = webClient.get()
                .uri("/personal/movilesByRegional/"+idRegional)
                .exchange()
                .expectStatus().isOk()
                .expectBody(new ParameterizedTypeReference<Response<List<Movil>>>(){})
                .returnResult().getResponseBody();

        Assertions.assertNotNull(response);
        Assertions.assertEquals(respuestaEsperada.getResult(), response.getResult());

    }

    @Test
    void consultarSecuenciasTurno(){
        List<SecuenciaTurno> secuenciaTurnos = new ArrayList<>();
        secuenciaTurnos.add(SecuenciaTurno.builder().descripcion("SSS0").build());

        Response<List<SecuenciaTurno>> respuestaEsperada = Response.<List<SecuenciaTurno>>builder()
                .result(secuenciaTurnos)
                .build();

        Mockito.when(secuenciasHorarioRepositoryMock.consultarSecuencias())
                .thenReturn(Flux.fromIterable(secuenciaTurnos));

        Response<List<SecuenciaTurno>> response = webClient.get()
                .uri("/personal/secuenciasTurno")
                .exchange()
                .expectStatus().isOk()
                .expectBody(new ParameterizedTypeReference<Response<List<SecuenciaTurno>>>(){})
                .returnResult().getResponseBody();

        Assertions.assertNotNull(response);
        Assertions.assertEquals(respuestaEsperada.getResult(), response.getResult());

    }

    @Test
    void eliminarTurnosProfesionalesAccionMasiva(){
        List<EliminarTurnoProfesionalRequest> eliminarTurnoProfesionalRequests = new ArrayList<>();
        eliminarTurnoProfesionalRequests.add(EliminarTurnoProfesionalRequest.builder().build());


        List<ResultadoActualizacionTurno> resultadoActualizacionTurnos = new ArrayList<>();
        resultadoActualizacionTurnos.add(ResultadoActualizacionTurno.builder().build());

        Response<List<ResultadoActualizacionTurno>> respuestaEsperada = Response.<List<ResultadoActualizacionTurno>>builder()
                .result(resultadoActualizacionTurnos)
                .build();

        Mockito.when(secuenciasHorarioRepositoryMock
                        .eliminarTurnosProfesionalesAccionMasiva(eliminarTurnoProfesionalRequests))
                .thenReturn(Flux.fromIterable(resultadoActualizacionTurnos));

        Response<List<ResultadoActualizacionTurno>> response = webClient.post()
                .uri("/personal/eliminarTurnosProfesionalesAccionMasiva")
                .bodyValue(eliminarTurnoProfesionalRequests)
                .exchange()
                .expectStatus().isOk()
                .expectBody(new ParameterizedTypeReference<Response<List<ResultadoActualizacionTurno>>>(){})
                .returnResult().getResponseBody();

        Assertions.assertNotNull(response);
        Assertions.assertEquals(respuestaEsperada.getResult(), response.getResult());

    }

    @Test
    void asignarTurnosProfesionalesAccionMasiva(){
        List<TurnoProfesional> turnoProfesionales = new ArrayList<>();
        turnoProfesionales.add(TurnoProfesional.builder().build());


        List<ResultadoActualizacionTurno> resultadoActualizacionTurnos = new ArrayList<>();
        resultadoActualizacionTurnos.add(ResultadoActualizacionTurno.builder().build());

        Response<List<ResultadoActualizacionTurno>> respuestaEsperada = Response.<List<ResultadoActualizacionTurno>>builder()
                .result(resultadoActualizacionTurnos)
                .build();

        Mockito.when(secuenciasHorarioRepositoryMock
                        .asignarTurnosProfesionalesAccionMasiva(turnoProfesionales))
                .thenReturn(Flux.fromIterable(resultadoActualizacionTurnos));

        Response<List<ResultadoActualizacionTurno>> response = webClient.post()
                .uri("/personal/asignarTurnosProfesionalesAccionMasiva")
                .bodyValue(turnoProfesionales)
                .exchange()
                .expectStatus().isOk()
                .expectBody(new ParameterizedTypeReference<Response<List<ResultadoActualizacionTurno>>>(){})
                .returnResult().getResponseBody();

        Assertions.assertNotNull(response);
        Assertions.assertEquals(respuestaEsperada.getResult(), response.getResult());

    }

    @Test
    void configurarSecuenciaTurno(){
        SecuenciaTurno secuenciaTurno = SecuenciaTurno.builder().descripcion("SS0").build();

        Mockito.when(secuenciasHorarioRepositoryMock
                        .configurarSecuenciaTurno(secuenciaTurno))
                .thenReturn(Mono.just(Boolean.TRUE));

        Response<Boolean> response = webClient.post()
                .uri("/personal/secuenciasTurno")
                .bodyValue(secuenciaTurno)
                .exchange()
                .expectStatus().isOk()
                .expectBody(new ParameterizedTypeReference<Response<Boolean>>(){})
                .returnResult().getResponseBody();

        Assertions.assertNotNull(response);
        Assertions.assertTrue( response.getResult());

    }
}
