package co.com.sura.web.remision;

import co.com.sura.constantes.Mensajes;
import co.com.sura.constantes.StatusCode;
import co.com.sura.genericos.Response;
import co.com.sura.remision.RemisionUseCase;
import co.com.sura.remision.dto.CitaRequest;
import co.com.sura.remision.dto.CrearRemisionCitasRequest;
import co.com.sura.remision.dto.NovedadRequest;
import co.com.sura.remision.dto.RemisionRequest;
import co.com.sura.remision.entity.Remision;
import co.com.sura.remision.entity.datosremision.DatosAtencionPaciente;
import co.com.sura.remision.entity.datosremision.Paciente;
import co.com.sura.remision.entity.historial.RegistroHistorialRemision;
import co.com.sura.remision.gateway.HistorialRemisionRepository;
import co.com.sura.remision.gateway.RemisionCrudRepository;
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

import java.util.ArrayList;
import java.util.List;

@ContextConfiguration(classes = RemisionController.class)
@WebFluxTest(controllers = RemisionController.class,excludeAutoConfiguration = {ReactiveSecurityAutoConfiguration.class})
@ExtendWith(SpringExtension.class)
@ExtendWith(MockitoExtension.class)
@Import(RemisionUseCase.class)
class RemisionControllerTest {

    private final String idRemision = "aff854as";
    @MockBean
    private RemisionCrudRepository remisionCrudRepositoryMock;

    @MockBean
    private HistorialRemisionRepository historialRemisionRepositoryMock;
    @Autowired
    private WebTestClient webClient;

    @Test
    void crearRemision(){
        RemisionRequest remisionRequest = RemisionRequest.builder().build();
        List<CitaRequest> citasRequest = new ArrayList<>();
        CrearRemisionCitasRequest crearRemisionCitasRequest = CrearRemisionCitasRequest
                .builder()
                .remision(remisionRequest)
                .citas(citasRequest)
                .build();
        citasRequest.add(CitaRequest.builder().build());
        Mockito.when(remisionCrudRepositoryMock.crearRemisionCita(remisionRequest,citasRequest))
                .thenReturn(Mono.just(Boolean.TRUE));


        Response<Boolean> response = webClient.post()
                .uri("/remision/crearRemisionCitas")
                .bodyValue(crearRemisionCitasRequest)
                .exchange()
                .expectStatus().isOk()
                .expectBody(new ParameterizedTypeReference<Response<Boolean>>(){})
                .returnResult().getResponseBody();

        Assertions.assertNotNull(response);
        Assertions.assertTrue(response.getResult());
    }
    @Test
    void crearRemisionError(){
        RemisionRequest remisionRequest = RemisionRequest.builder().build();
        List<CitaRequest> citasRequest = new ArrayList<>();
        CrearRemisionCitasRequest crearRemisionCitasRequest = CrearRemisionCitasRequest
                .builder()
                .remision(remisionRequest)
                .citas(citasRequest)
                .build();
        citasRequest.add(CitaRequest.builder().build());
        Mockito.when(remisionCrudRepositoryMock.crearRemisionCita(remisionRequest,citasRequest))
                .thenReturn(Mono.error(new Exception(Mensajes.ERROR_CREAR_REMISION)));


        Response<?> response = webClient.post()
                .uri("/remision/crearRemisionCitas")
                .bodyValue(crearRemisionCitasRequest)
                .exchange()
                .expectBody(Response.class)
                .returnResult().getResponseBody();

        Assertions.assertNotNull(response);
        Assertions.assertEquals(Mensajes.PETICION_FALLIDA, response.getMessage());
        Assertions.assertEquals(StatusCode.STATUS_500, response.getStatus());

    }
    @Test
    void actualizarRemisionPorNovedad(){
        RemisionRequest remisionRequest = RemisionRequest.builder().build();
        List<CitaRequest> citasRequest = new ArrayList<>();
        citasRequest.add(CitaRequest.builder().build());
        NovedadRequest novedadRequest = NovedadRequest.builder().build();

        CrearRemisionCitasRequest crearRemisionCitasRequest = CrearRemisionCitasRequest
                .builder()
                .remision(remisionRequest)
                .novedad(novedadRequest)
                .citas(citasRequest)
                .build();



        Mockito.when(remisionCrudRepositoryMock.actualizarRemisionPorNovedad(
                                   remisionRequest,citasRequest,novedadRequest))
                .thenReturn(Mono.just(Boolean.TRUE));


        Response<Boolean> response = webClient.post()
                .uri("/remision/actualizarRemisionPorNovedad")
                .bodyValue(crearRemisionCitasRequest)
                .exchange()
                .expectStatus().isOk()
                .expectBody(new ParameterizedTypeReference<Response<Boolean>>(){})
                .returnResult().getResponseBody();

        Assertions.assertNotNull(response);
        Assertions.assertTrue(response.getResult());
    }
    @Test
    void actualizarRemisionPorNovedadError(){
        RemisionRequest remisionRequest = RemisionRequest.builder().build();
        List<CitaRequest> citasRequest = new ArrayList<>();
        citasRequest.add(CitaRequest.builder().build());
        NovedadRequest novedadRequest = NovedadRequest.builder().build();

        CrearRemisionCitasRequest crearRemisionCitasRequest = CrearRemisionCitasRequest
                .builder()
                .remision(remisionRequest)
                .novedad(novedadRequest)
                .citas(citasRequest)
                .build();

        Mockito.when(remisionCrudRepositoryMock.actualizarRemisionPorNovedad(
                        remisionRequest,citasRequest,novedadRequest))
                .thenReturn(Mono.error(Exception::new));

        Response<?> response = webClient.post()
                .uri("/remision/actualizarRemisionPorNovedad")
                .bodyValue(crearRemisionCitasRequest)
                .exchange()
                .expectBody(Response.class)
                .returnResult().getResponseBody();

        Assertions.assertNotNull(response);
        Assertions.assertEquals(Mensajes.PETICION_FALLIDA, response.getMessage());
        Assertions.assertEquals(StatusCode.STATUS_500, response.getStatus());

    }
    @Test
    void consultarRemisiones(){
        List<Remision> remisiones = new ArrayList<>();
        remisiones.add(Remision.builder().idRemision(idRemision).build());

        Response<List<Remision>> respuestaEsperada = Response.<List<Remision>>builder()
                .result(remisiones)
                .build();

        Mockito.when(remisionCrudRepositoryMock.consultarRemisiones())
                .thenReturn(Flux.fromIterable(remisiones));



        Response<List<Remision>> response = webClient.get()
                .uri("/remision")
                .exchange()
                .expectStatus().isOk()
                .expectBody(new ParameterizedTypeReference<Response<List<Remision>>>(){})
                .returnResult().getResponseBody();

        Assertions.assertNotNull(response);
        Assertions.assertEquals(respuestaEsperada.getResult(), response.getResult());
    }
    @Test
    void consultarRemisionesError(){

        Mockito.when(remisionCrudRepositoryMock.consultarRemisiones())
                .thenReturn(Flux.error(Exception::new));



        Response<?> response = webClient.get()
                .uri("/remision")
                .exchange()
                .expectStatus().isOk()
                .expectBody(Response.class)
                .returnResult().getResponseBody();

        Assertions.assertNotNull(response);
        Assertions.assertEquals(Mensajes.PETICION_FALLIDA, response.getMessage());
        Assertions.assertEquals(StatusCode.STATUS_500, response.getStatus());

    }
    @Test
    void consultarDatosAtencionPacienteByRemision(){
        DatosAtencionPaciente datosAtencionPaciente = DatosAtencionPaciente.builder().build();

        Response<DatosAtencionPaciente> respuestaEsperada = Response.<DatosAtencionPaciente>builder()
                .result(datosAtencionPaciente)
                .build();

        Mockito.when(remisionCrudRepositoryMock.consultarDatosAtencionPacienteByIdRemision(idRemision))
                .thenReturn(Mono.just(datosAtencionPaciente));


        String url = "/remision/datosAtencionPaciente/"+idRemision;
        Response<DatosAtencionPaciente> response = webClient.get()
                .uri(url)
                .exchange()
                .expectStatus().isOk()
                .expectBody(new ParameterizedTypeReference<Response<DatosAtencionPaciente>>(){})
                .returnResult().getResponseBody();

        Assertions.assertNotNull(response);
        Assertions.assertEquals(respuestaEsperada.getResult(), response.getResult());
    }
    @Test
    void consultarDatosAtencionPacienteByRemisionError(){
        DatosAtencionPaciente datosAtencionPaciente = DatosAtencionPaciente.builder().build();

        Mockito.when(remisionCrudRepositoryMock.consultarDatosAtencionPacienteByIdRemision(idRemision))
                .thenReturn(Mono.error(Exception::new));


        String url = "/remision/datosAtencionPaciente/"+idRemision;
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
    void consultarPacienteFromRemision(){
        Paciente paciente = Paciente.builder().build();

        Response<Paciente> respuestaEsperada = Response.<Paciente>builder()
                .result(paciente)
                .build();

        Mockito.when(remisionCrudRepositoryMock.consultarPacienteFromRemision(idRemision))
                .thenReturn(Mono.just(paciente));


        String url = "/remision/pacienteRemision/"+idRemision;
        Response<Paciente> response = webClient.get()
                .uri(url)
                .exchange()
                .expectStatus().isOk()
                .expectBody(new ParameterizedTypeReference<Response<Paciente>>(){})
                .returnResult().getResponseBody();

        Assertions.assertNotNull(response);
        Assertions.assertEquals(respuestaEsperada.getResult(), response.getResult());
    }
    @Test
    void consultarPacienteFromRemisionError(){

        Mockito.when(remisionCrudRepositoryMock.consultarPacienteFromRemision(idRemision))
                .thenReturn(Mono.error(Exception::new));


        String url = "/remision/pacienteRemision/"+idRemision;
        Response<?> response = webClient.get()
                .uri(url)
                .exchange()
                .expectStatus().isOk()
                .expectBody(Response.class)
                .returnResult().getResponseBody();

        Assertions.assertNotNull(response);
        Assertions.assertEquals(Mensajes.PETICION_FALLIDA, response.getMessage());
        Assertions.assertEquals(StatusCode.STATUS_500, response.getStatus());

    }
    @Test
    void consultarHistorialRemisionById(){
        List<RegistroHistorialRemision> historialRemision = new ArrayList<>();
        historialRemision.add(RegistroHistorialRemision.builder().idRemision(idRemision).build());

        Response<List<RegistroHistorialRemision>> respuestaEsperada = Response.<List<RegistroHistorialRemision>>builder()
                .result(historialRemision)
                .build();

        Mockito.when(historialRemisionRepositoryMock.consultarHistoricoRemision(idRemision))
                .thenReturn(Flux.fromIterable(historialRemision));


        String url = "/remision/historial/"+idRemision;
        Response<List<RegistroHistorialRemision>> response = webClient.get()
                .uri(url)
                .exchange()
                .expectStatus().isOk()
                .expectBody(new ParameterizedTypeReference<Response<List<RegistroHistorialRemision>>>(){})
                .returnResult().getResponseBody();

        Assertions.assertNotNull(response);
        Assertions.assertEquals(respuestaEsperada.getResult(), response.getResult());
    }
    @Test
    void consultarHistorialRemisionByIdError(){

        Mockito.when(historialRemisionRepositoryMock.consultarHistoricoRemision(idRemision))
                .thenReturn(Flux.error(Exception::new));

        String url = "/remision/historial/"+idRemision;
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
    void consultarAllDataRemisionById(){
        RegistroHistorialRemision historialRemision = RegistroHistorialRemision.builder().idRemision(idRemision).build();


        Response<RegistroHistorialRemision> respuestaEsperada = Response.<RegistroHistorialRemision>builder()
                .result(historialRemision)
                .build();

        Mockito.when(historialRemisionRepositoryMock.consultarDatosRemision(idRemision))
                .thenReturn(Mono.just(historialRemision));


        String url = "/remision/"+idRemision;
        Response<RegistroHistorialRemision> response = webClient.get()
                .uri(url)
                .exchange()
                .expectStatus().isOk()
                .expectBody(new ParameterizedTypeReference<Response<RegistroHistorialRemision>>(){})
                .returnResult().getResponseBody();

        Assertions.assertNotNull(response);
        Assertions.assertEquals(respuestaEsperada.getResult(), response.getResult());
    }
    @Test
    void consultarAllDataRemisionByIdError(){

        Mockito.when(historialRemisionRepositoryMock.consultarDatosRemision(idRemision))
                .thenReturn(Mono.error(Exception::new));


        String url = "/remision/"+idRemision;
        Response<?> response = webClient.get()
                .uri(url)
                .exchange()
                .expectBody(Response.class)
                .returnResult().getResponseBody();

        Assertions.assertNotNull(response);
        Assertions.assertEquals(Mensajes.PETICION_FALLIDA, response.getDetail());
        Assertions.assertEquals(StatusCode.STATUS_500, response.getStatus());

    }
    @Test
    void egresarRemisionById(){

        Mockito.when(remisionCrudRepositoryMock.egresarRemisionById(idRemision))
                .thenReturn(Mono.just(Boolean.TRUE));


        UriComponentsBuilder builderUrl = UriComponentsBuilder
                .fromPath("/remision/egresar")
                .queryParam("idRemision", idRemision);
        Response<Boolean> response = webClient.put()
                .uri(builderUrl.build().toUri())
                .exchange()
                .expectStatus().isOk()
                .expectBody(new ParameterizedTypeReference<Response<Boolean>>(){})
                .returnResult().getResponseBody();

        Assertions.assertNotNull(response);
        Assertions.assertTrue(response.getResult());
    }
    @Test
    void egresarRemisionByIdError(){

        Mockito.when(remisionCrudRepositoryMock.egresarRemisionById(idRemision))
                .thenReturn(Mono.error(Exception::new));

        UriComponentsBuilder builderUrl = UriComponentsBuilder
                .fromPath("/remision/egresar")
                .queryParam("idRemision", idRemision);

        Response<?> response = webClient.put()
                .uri(builderUrl.build().toUri())
                .exchange()
                .expectBody(Response.class)
                .returnResult().getResponseBody();

        Assertions.assertNotNull(response);
        Assertions.assertEquals(Mensajes.ERROR_EGRESAR_REMISION, response.getDetail());
        Assertions.assertEquals(StatusCode.STATUS_500, response.getStatus());

    }

}
