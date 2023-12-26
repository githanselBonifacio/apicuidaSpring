package co.com.sura.web.maestro;

import co.com.sura.constantes.Mensajes;
import co.com.sura.constantes.StatusCode;
import co.com.sura.genericos.Response;
import co.com.sura.maestro.CrudMaestroUseCase;
import co.com.sura.maestros.entity.*;
import co.com.sura.maestros.gateway.MaestroRepository;
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
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import java.util.ArrayList;
import java.util.List;

@ContextConfiguration(classes = MaestroController.class)
@WebFluxTest(controllers = MaestroController.class,excludeAutoConfiguration = {ReactiveSecurityAutoConfiguration.class})
@ExtendWith(SpringExtension.class)
@ExtendWith(MockitoExtension.class)
@Import(CrudMaestroUseCase.class)
class MaestrosControllerTest {

    @MockBean
    private MaestroRepository maestroRepositoryMock;
    @Autowired
    private WebTestClient webClient;


    @Test
    void consultarRegional(){

        List<Regional> regionales = new ArrayList<>();
        regionales.add(Regional.builder().build());

        Response<List<Regional>> respuestaEsperada = Response.<List<Regional>>builder()
                .result(regionales)
                .build();

        Mockito.when(maestroRepositoryMock.consultarRegional())
                        .thenReturn(Flux.fromIterable(regionales));

        Response<List<Regional>> response = webClient.get()
                .uri("/maestros/regionales")
                .exchange()
                .expectStatus().isOk()
                .expectBody(new ParameterizedTypeReference<Response<List<Regional>>>(){})
                .returnResult().getResponseBody();

        Assertions.assertNotNull(response);
        Assertions.assertEquals(respuestaEsperada.getResult(),response.getResult());

    }
    @Test
    void consultarRegionalError(){
        Mockito.when(maestroRepositoryMock.consultarRegional())
                .thenReturn(Flux.error(Exception::new));

        Response<?> response = webClient.get()
                .uri("/maestros/regionales")
                .exchange()
                .expectBody(Response.class)
                .returnResult().getResponseBody();

        Assertions.assertNotNull(response);
        Assertions.assertEquals(Mensajes.PETICION_FALLIDA, response.getMessage());
        Assertions.assertEquals(StatusCode.STATUS_500, response.getStatus());
    }
    @Test
    void consultarRegionalById(){
        String idRegional = "427";
        Regional regional = Regional.builder().build();

        Response<Regional> respuestaEsperada = Response.<Regional>builder()
                .result(regional)
                .build();

        Mockito.when(maestroRepositoryMock.consultarRegionalById(idRegional))
                .thenReturn(Mono.just(regional));

        String url = "/maestros/regionales/"+idRegional;
        Response<Regional> response = webClient.get()
                .uri(url)
                .exchange()
                .expectStatus().isOk()
                .expectBody(new ParameterizedTypeReference<Response<Regional>>(){})
                .returnResult().getResponseBody();

        Assertions.assertNotNull(response);
        Assertions.assertEquals(respuestaEsperada.getResult(),response.getResult());

    }
    @Test
    void consultarRegionalByIdError(){
        String idRegional = "427";
        Mockito.when(maestroRepositoryMock.consultarRegionalById(idRegional))
                .thenReturn(Mono.error(Exception::new));

        String url = "/maestros/regionales/"+idRegional;
        Response<?> response = webClient.get()
                .uri(url)
                .exchange()
                .expectBody(Response.class)
                .returnResult().getResponseBody();

        Assertions.assertNotNull(response);
        Assertions.assertEquals(Mensajes.PETICION_FALLIDA, response.getMessage());
        Assertions.assertEquals(StatusCode.STATUS_500, response.getStatus());

    }

    @Test
    void consultarHorarioTurno(){

        List<HorarioTurno> horarioTurnos = new ArrayList<>();
        horarioTurnos.add(HorarioTurno.builder().build());

        Response<List<HorarioTurno>> respuestaEsperada = Response.<List<HorarioTurno>>builder()
                .result(horarioTurnos)
                .build();

        Mockito.when(maestroRepositoryMock.consultarHorarioTurno())
                .thenReturn(Flux.fromIterable(horarioTurnos));

        Response<List<HorarioTurno>> response = webClient.get()
                .uri("/maestros/horarioTurno")
                .exchange()
                .expectStatus().isOk()
                .expectBody(new ParameterizedTypeReference<Response<List<HorarioTurno>>>(){})
                .returnResult().getResponseBody();

        Assertions.assertNotNull(response);
        Assertions.assertEquals(respuestaEsperada.getResult(),response.getResult());

    }
    @Test
    void consultarHorarioTurnoError(){

        Mockito.when(maestroRepositoryMock.consultarHorarioTurno())
                .thenReturn(Flux.error(Exception::new));

        Response<?> response = webClient.get()
                .uri("/maestros/horarioTurno")
                .exchange()
                .expectBody(Response.class)
                .returnResult().getResponseBody();

        Assertions.assertNotNull(response);
        Assertions.assertEquals(Mensajes.PETICION_FALLIDA, response.getMessage());
        Assertions.assertEquals(StatusCode.STATUS_500, response.getStatus());


    }
    @Test
    void consultarHorarioTurnoById(){
        Integer idHorarioTurno = 1;

        HorarioTurno horarioTurno = HorarioTurno.builder().build();

        Response<HorarioTurno> respuestaEsperada = Response.<HorarioTurno>builder()
                .result(horarioTurno)
                .build();
        Mockito.when(maestroRepositoryMock.consultarHorarioTurnoById(idHorarioTurno))
                .thenReturn(Mono.just(horarioTurno));

        String url = "/maestros/horarioTurno/"+idHorarioTurno;
        Response<HorarioTurno> response = webClient.get()
                .uri(url)
                .exchange()
                .expectStatus().isOk()
                .expectBody(new ParameterizedTypeReference<Response<HorarioTurno>>(){})
                .returnResult().getResponseBody();

        Assertions.assertNotNull(response);
        Assertions.assertEquals(respuestaEsperada.getResult(),response.getResult());

    }
    @Test
    void consultarHorarioTurnoByIdError(){
        Integer idHorarioTurno = 1;

        Mockito.when(maestroRepositoryMock.consultarHorarioTurnoById(idHorarioTurno))
                .thenReturn(Mono.error(Exception::new));

        String url = "/maestros/horarioTurno/"+idHorarioTurno;
        Response<?> response = webClient.get()
                .uri(url)
                .exchange()
                .expectBody(Response.class)
                .returnResult().getResponseBody();

        Assertions.assertNotNull(response);
        Assertions.assertEquals(Mensajes.PETICION_FALLIDA, response.getMessage());
        Assertions.assertEquals(StatusCode.STATUS_500, response.getStatus());

    }

    @Test
    void consultarTipoIdentificacion(){

        List<TipoIdentificacion> tipoIdentificacion = new ArrayList<>();
        tipoIdentificacion.add(TipoIdentificacion.builder().build());

        Response<List<TipoIdentificacion>> respuestaEsperada = Response.<List<TipoIdentificacion>>builder()
                .result(tipoIdentificacion)
                .build();

        Mockito.when(maestroRepositoryMock.consultarTipoIdentificacion())
                .thenReturn(Flux.fromIterable(tipoIdentificacion));

        Response<List<TipoIdentificacion>> response = webClient.get()
                .uri("/maestros/tipoIdentificacion")
                .exchange()
                .expectStatus().isOk()
                .expectBody(new ParameterizedTypeReference<Response<List<TipoIdentificacion>>>(){})
                .returnResult().getResponseBody();

        Assertions.assertNotNull(response);
        Assertions.assertEquals(respuestaEsperada.getResult(),response.getResult());

    }
    @Test
    void consultarTipoIdentificacionError(){
        Mockito.when(maestroRepositoryMock.consultarTipoIdentificacion())
                .thenReturn(Flux.error(Exception::new));

        Response<?> response = webClient.get()
                .uri("/maestros/tipoIdentificacion")
                .exchange()
                .expectBody(Response.class)
                .returnResult().getResponseBody();

        Assertions.assertNotNull(response);
        Assertions.assertEquals(Mensajes.PETICION_FALLIDA, response.getMessage());
        Assertions.assertEquals(StatusCode.STATUS_500, response.getStatus());

    }
    @Test
    void consultarTipoIdentificacionById(){
        Integer idTipoIdentificacion = 1;
        TipoIdentificacion tipoIdentificacion = TipoIdentificacion.builder().build();

        Response<TipoIdentificacion> respuestaEsperada = Response.<TipoIdentificacion>builder()
                .result(tipoIdentificacion)
                .build();

        Mockito.when(maestroRepositoryMock.consultarTipoIdentificacionById(idTipoIdentificacion))
                .thenReturn(Mono.just(tipoIdentificacion));

        String url = "/maestros/tipoIdentificacion/"+idTipoIdentificacion;
        Response<TipoIdentificacion> response = webClient.get()
                .uri(url)
                .exchange()
                .expectStatus().isOk()
                .expectBody(new ParameterizedTypeReference<Response<TipoIdentificacion>>(){})
                .returnResult().getResponseBody();

        Assertions.assertNotNull(response);
        Assertions.assertEquals(respuestaEsperada.getResult(),response.getResult());

    }
    @Test
    void consultarTipoIdentificacionByIdError(){
        Integer idTipoIdentificacion = 1;


        Mockito.when(maestroRepositoryMock.consultarTipoIdentificacionById(idTipoIdentificacion))
                .thenReturn(Mono.error(Exception::new));

        String url = "/maestros/tipoIdentificacion/"+idTipoIdentificacion;
        Response<?> response = webClient.get()
                .uri(url)
                .exchange()
                .expectBody(Response.class)
                .returnResult().getResponseBody();

        Assertions.assertNotNull(response);
        Assertions.assertEquals(Mensajes.PETICION_FALLIDA, response.getMessage());
        Assertions.assertEquals(StatusCode.STATUS_500, response.getStatus());

    }
    @Test
    void consultarEstadocita(){

        List<EstadoCita> estadosCitas = new ArrayList<>();
        estadosCitas.add(EstadoCita.builder().build());

        Response<List<EstadoCita>> respuestaEsperada = Response.<List<EstadoCita>>builder()
                .result(estadosCitas)
                .build();

        Mockito.when(maestroRepositoryMock.consultarEstadosCita())
                .thenReturn(Flux.fromIterable(estadosCitas));

        Response<List<EstadoCita>> response = webClient.get()
                .uri("/maestros/estadosCita")
                .exchange()
                .expectStatus().isOk()
                .expectBody(new ParameterizedTypeReference<Response<List<EstadoCita>>>(){})
                .returnResult().getResponseBody();

        Assertions.assertNotNull(response);
        Assertions.assertEquals(respuestaEsperada.getResult(),response.getResult());

    }

    @Test
    void consultarEstadocitaError(){

        Mockito.when(maestroRepositoryMock.consultarEstadosCita())
                .thenReturn(Flux.error(Exception::new));

        Response<?> response = webClient.get()
                .uri("/maestros/estadosCita")
                .exchange()
                .expectBody(Response.class)
                .returnResult().getResponseBody();

        Assertions.assertNotNull(response);
        Assertions.assertEquals(Mensajes.PETICION_FALLIDA, response.getMessage());
        Assertions.assertEquals(StatusCode.STATUS_500, response.getStatus());


    }

    @Test
    void consultarProfesiones(){

        List<Profesion> profesiones = new ArrayList<>();
        profesiones.add(Profesion.builder().build());

        Response<List<Profesion>> respuestaEsperada = Response.<List<Profesion>>builder()
                .result(profesiones)
                .build();

        Mockito.when(maestroRepositoryMock.consultarProfesiones())
                .thenReturn(Flux.fromIterable(profesiones));

        Response<List<Profesion>> response = webClient.get()
                .uri("/maestros/profesiones")
                .exchange()
                .expectStatus().isOk()
                .expectBody(new ParameterizedTypeReference<Response<List<Profesion>>>() {})
                .returnResult().getResponseBody();

        Assertions.assertNotNull(response);
        Assertions.assertEquals(respuestaEsperada.getResult(),response.getResult());

    }
    @Test
    void consultarProfesionesError(){

        Mockito.when(maestroRepositoryMock.consultarProfesiones())
                .thenReturn(Flux.error(Exception::new));

        Response<?> response = webClient.get()
                .uri("/maestros/profesiones")
                .exchange()
                .expectBody(Response.class)
                .returnResult().getResponseBody();

        Assertions.assertNotNull(response);
        Assertions.assertEquals(Mensajes.PETICION_FALLIDA, response.getMessage());
        Assertions.assertEquals(StatusCode.STATUS_500, response.getStatus());

    }
}
