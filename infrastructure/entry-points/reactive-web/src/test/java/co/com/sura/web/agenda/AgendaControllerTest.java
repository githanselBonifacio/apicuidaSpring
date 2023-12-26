package co.com.sura.web.agenda;

import co.com.sura.agenda.AgendaUseCase;
import co.com.sura.agenda.entity.Actividad;
import co.com.sura.agenda.entity.Cita;
import co.com.sura.agenda.gateway.AgendaRepository;
import co.com.sura.agenda.gateway.AgendamientoAutomaticoRepository;
import co.com.sura.agenda.gateway.GestionEstadosCitasRepository;
import co.com.sura.constantes.Mensajes;
import co.com.sura.constantes.StatusCode;
import co.com.sura.genericos.Response;
import co.com.sura.moviles.entity.Desplazamiento;
import co.com.sura.personal.entity.Profesional;
import co.com.sura.personal.entity.TurnoProfesional;
import co.com.sura.personal.gateway.PersonalCrudRepository;
import co.com.sura.remision.entity.datosremision.Tratamiento;
import co.com.sura.remision.entity.procedimientos.Procedimientos;
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
import java.time.LocalDateTime;
import java.util.*;

@ContextConfiguration(classes = AgendaController.class)
@WebFluxTest(controllers = AgendaController.class,excludeAutoConfiguration = {ReactiveSecurityAutoConfiguration.class})
@ExtendWith(SpringExtension.class)
@ExtendWith(MockitoExtension.class)
@Import(AgendaUseCase.class)
 class AgendaControllerTest {
    private final LocalDate fechaTurno = LocalDate.of(2023,7,7);

    private final LocalDateTime fechaProgramada = LocalDateTime.of(2023,7,7,8,10);

    private final String idRegional = "427";

    private final String idCita = "hjdf54fd-1";
    private final String idProfesional = "989898";
    private final Integer idHorarioTurno = 1;

    @MockBean
    private AgendaRepository agendaRepositoryMock;
    @MockBean
    private  PersonalCrudRepository personalCrudRepositoryMock;
    @MockBean
    private  GestionEstadosCitasRepository gestionEstadosCitasRepositoryMock;
    @MockBean
    private  AgendamientoAutomaticoRepository agendamientoAutomaticoRepositoryMock;

    @Autowired
    private WebTestClient webClient;

    @Test
    void getProfesionalesbyTurnoRegional(){
        List<Profesional> profesionales = new ArrayList<>();
        profesionales.add(Profesional.builder().numeroIdentificacion("989898").build());

        Response<List<Profesional>> respuestaEsperada = Response.<List<Profesional>>builder()
                .result(profesionales)
                .build();

        Mockito.when(personalCrudRepositoryMock.consultarProfesionalByTurnoRegional(fechaTurno,idRegional))
                .thenReturn(Flux.fromIterable(profesionales));

        UriComponentsBuilder builderUrl = UriComponentsBuilder.fromPath("/agenda/profesionalesByTurnoRegional")
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
    void getProfesionalesbyTurnoRegionalError(){
        Mockito.when(personalCrudRepositoryMock.consultarProfesionalByTurnoRegional(fechaTurno,idRegional))
                .thenReturn(Flux.error(Exception::new));

        UriComponentsBuilder builderUrl = UriComponentsBuilder.fromPath("/agenda/profesionalesByTurnoRegional")
                .queryParam("fechaTurno", fechaTurno)
                .queryParam("idRegional", idRegional);

        Response<?> response = webClient.get()
                .uri(builderUrl.build().toUri())
                .exchange()
                .expectBody(Response.class)
                .returnResult().getResponseBody();

        Assertions.assertNotNull(response);
        Assertions.assertEquals(Mensajes.PETICION_FALLIDA, response.getMessage());
        Assertions.assertEquals(StatusCode.STATUS_500, response.getStatus());
    }

    @Test
    void getProfesionalesfromTurnoRegional(){
        List<Profesional> profesionales = new ArrayList<>();
        profesionales.add(Profesional.builder().numeroIdentificacion("989898").build());

        Response<List<Profesional>> respuestaEsperada = Response.<List<Profesional>>builder()
                .result(profesionales)
                .build();

        Mockito.when(personalCrudRepositoryMock.consultarProfesionalFromTurnoRegional(fechaTurno,idRegional,idHorarioTurno))
                .thenReturn(Flux.fromIterable(profesionales));

        UriComponentsBuilder builderUrl = UriComponentsBuilder.fromPath("/agenda/profesionalesFromTurnoRegional")
                .queryParam("fechaTurno", fechaTurno)
                .queryParam("idRegional", idRegional)
                .queryParam("idHorarioTurno", idHorarioTurno);

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
    void getProfesionalesfromTurnoRegionalError(){
        Mockito.when(personalCrudRepositoryMock.consultarProfesionalFromTurnoRegional(fechaTurno,idRegional,idHorarioTurno))
                .thenReturn(Flux.error(Exception::new));

        UriComponentsBuilder builderUrl = UriComponentsBuilder.fromPath("/agenda/profesionalesFromTurnoRegional")
                .queryParam("fechaTurno", fechaTurno)
                .queryParam("idRegional", idRegional)
                .queryParam("idHorarioTurno", idHorarioTurno);

        Response<?> response = webClient.get()
                .uri(builderUrl.build().toUri())
                .exchange()
                .expectBody(Response.class)
                .returnResult().getResponseBody();

        Assertions.assertNotNull(response);
        Assertions.assertEquals(Mensajes.PETICION_FALLIDA, response.getMessage());
        Assertions.assertEquals(StatusCode.STATUS_500, response.getStatus());
    }
    @Test
    void consultarProfesionalesByRegional(){
        List<Profesional> profesionales = new ArrayList<>();
        profesionales.add(Profesional.builder().numeroIdentificacion("989898").build());

        Response<List<Profesional>> respuestaEsperada = Response.<List<Profesional>>builder()
                .result(profesionales)
                .build();

        Mockito.when(personalCrudRepositoryMock.consultarProfesionalesByIdRegional(idRegional))
                .thenReturn(Flux.fromIterable(profesionales));


        String url = "/agenda/profesionales/"+idRegional;
        Response<List<Profesional>> response = webClient.get()
                .uri(url)
                .exchange()
                .expectStatus().isOk()
                .expectBody(new ParameterizedTypeReference<Response<List<Profesional>>>(){})
                .returnResult().getResponseBody();

        Assertions.assertNotNull(response);
        Assertions.assertEquals(respuestaEsperada.getResult(), response.getResult());
    }
    @Test
    void consultarProfesionalesByRegionalError(){
        Mockito.when(personalCrudRepositoryMock.consultarProfesionalesByIdRegional(idRegional))
                .thenReturn(Flux.error(Exception::new));

        String url = "/agenda/profesionales/"+idRegional;
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
    void asignarProfesionalTurno(){
        TurnoProfesional turnoProfesional = TurnoProfesional.builder()
                .idProfesional("989897")
                .fechaTurno(fechaTurno)
                .idHorarioTurno(idHorarioTurno)
                .build();


        Mockito.when(agendaRepositoryMock.asignarProfesionalTurno(turnoProfesional))
                .thenReturn(Mono.just(Boolean.TRUE));


        String url = "/agenda/asignarProfesionalTurno";
        Response<Boolean> response = webClient.post()
                .uri(url)
                .bodyValue(turnoProfesional)
                .exchange()
                .expectStatus().isOk()
                .expectBody(new ParameterizedTypeReference<Response<Boolean>>(){})
                .returnResult().getResponseBody();

        Assertions.assertNotNull(response);
        Assertions.assertTrue(response.getResult());
    }
    @Test
    void asignarProfesionalTurnoErro(){
        TurnoProfesional turnoProfesional = TurnoProfesional.builder()
                .idProfesional("989897")
                .fechaTurno(fechaTurno)
                .idHorarioTurno(idHorarioTurno)
                .build();

        Mockito.when(agendaRepositoryMock.asignarProfesionalTurno(turnoProfesional))
                .thenReturn(Mono.error(Exception::new));

        String url = "/agenda/asignarProfesionalTurno";
        Response<?> response = webClient.post()
                .uri(url)
                .bodyValue(turnoProfesional)
                .exchange()
                .expectBody(Response.class)
                .returnResult().getResponseBody();

        Assertions.assertNotNull(response);
        Assertions.assertEquals(Mensajes.PETICION_FALLIDA, response.getMessage());
        Assertions.assertEquals(StatusCode.STATUS_500, response.getStatus());
    }
    @Test
    void desasignarProfesionalTurno(){
        TurnoProfesional turnoProfesional = TurnoProfesional.builder()
                .idProfesional("989897")
                .fechaTurno(fechaTurno)
                .idHorarioTurno(idHorarioTurno)
                .build();


        Mockito.when(agendaRepositoryMock.desasignarProfesionalTurno(turnoProfesional))
                .thenReturn(Mono.just(Boolean.TRUE));


        String url = "/agenda/desasignarProfesionalTurno";
        Response<Boolean> response = webClient.post()
                .uri(url)
                .bodyValue(turnoProfesional)
                .exchange()
                .expectStatus().isOk()
                .expectBody(new ParameterizedTypeReference<Response<Boolean>>(){})
                .returnResult().getResponseBody();

        Assertions.assertNotNull(response);
        Assertions.assertTrue(response.getResult());
    }
    @Test
    void desasignarProfesionalTurnoError(){

        TurnoProfesional turnoProfesional = TurnoProfesional.builder()
                .idProfesional("989897")
                .fechaTurno(fechaTurno)
                .idHorarioTurno(idHorarioTurno)
                .build();

        Mockito.when(agendaRepositoryMock.desasignarProfesionalTurno(turnoProfesional))
                .thenReturn(Mono.error(Exception::new));

        String url = "/agenda/desasignarProfesionalTurno";
        Response<?> response = webClient.post()
                .uri(url)
                .bodyValue(turnoProfesional)
                .exchange()
                .expectBody(Response.class)
                .returnResult().getResponseBody();

        Assertions.assertNotNull(response);
        Assertions.assertEquals(Mensajes.NO_DESASIGNO_PROFESIONAL_TURNO, response.getDetail());
        Assertions.assertEquals(StatusCode.STATUS_500, response.getStatus());
    }
    @Test
    void desagendarTurnoCompleto(){

        Mockito.when(agendamientoAutomaticoRepositoryMock.desagendarTurnoCompleto(
                        fechaTurno,idHorarioTurno,idRegional
                ))
                .thenReturn(Mono.just(Boolean.TRUE));


        UriComponentsBuilder builderUrl = UriComponentsBuilder.fromPath("/agenda/desagendarTurnoCompleto")
                .queryParam("fechaTurno", fechaTurno)
                .queryParam("idRegional", idRegional)
                .queryParam("idHorarioTurno", idHorarioTurno);

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
    void desagendarTurnoCompletoError(){

        Mockito.when(agendamientoAutomaticoRepositoryMock.desagendarTurnoCompleto(
                        fechaTurno,idHorarioTurno,idRegional))
                .thenReturn(Mono.error(Exception::new));

        UriComponentsBuilder builderUrl = UriComponentsBuilder.fromPath("/agenda/desagendarTurnoCompleto")
                .queryParam("fechaTurno", fechaTurno)
                .queryParam("idRegional", idRegional)
                .queryParam("idHorarioTurno", idHorarioTurno);

        Response<?> response = webClient.put()
                .uri(builderUrl.build().toUri())
                .exchange()
                .expectBody(Response.class)
                .returnResult().getResponseBody();

        Assertions.assertNotNull(response);
        Assertions.assertEquals(Mensajes.PETICION_FALLIDA, response.getMessage());
        Assertions.assertEquals(StatusCode.STATUS_500, response.getStatus());

    }
    @Test
    void autoagendarTurnoCompleto(){

        Mockito.when(agendamientoAutomaticoRepositoryMock.autoagendarTurnoCompleto(
                        fechaTurno,idHorarioTurno,idRegional
                ))
                .thenReturn(Mono.just(Boolean.TRUE));


        UriComponentsBuilder builderUrl = UriComponentsBuilder.fromPath("/agenda/autoagendarTurnoCompleto")
                .queryParam("fechaTurno", fechaTurno)
                .queryParam("idRegional", idRegional)
                .queryParam("idHorarioTurno", idHorarioTurno);

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
    void autoagendarTurnoCompletoError(){
        Mockito.when(agendamientoAutomaticoRepositoryMock.autoagendarTurnoCompleto(
                        fechaTurno,idHorarioTurno,idRegional))
                .thenReturn(Mono.error(Exception::new));


        UriComponentsBuilder builderUrl = UriComponentsBuilder.fromPath("/agenda/autoagendarTurnoCompleto")
                .queryParam("fechaTurno", fechaTurno)
                .queryParam("idRegional", idRegional)
                .queryParam("idHorarioTurno", idHorarioTurno);

        Response<?> response = webClient.put()
                .uri(builderUrl.build().toUri())
                .exchange()
                .expectBody(Response.class)
                .returnResult().getResponseBody();

        Assertions.assertNotNull(response);
        Assertions.assertEquals(Mensajes.ERROR_AUTOAGENDADO, response.getMessage());
        Assertions.assertEquals(StatusCode.STATUS_500, response.getStatus());
    }
    @Test
    void consultarActividadesProfesionalesRegionalHorario(){
        List<Actividad> actividades = new ArrayList<>();
        actividades.add(Actividad.builder().numeroIdentificacion("989898").build());

        Response<List<Actividad>> respuestaEsperada = Response.<List<Actividad>>builder()
                .result(actividades)
                .build();

        Mockito.when(agendaRepositoryMock.consultarActividadesByProfesionalesRegionalHorarioTurno(
                fechaTurno,idHorarioTurno,idRegional))
                .thenReturn(Flux.fromIterable(actividades));

        UriComponentsBuilder builderUrl = UriComponentsBuilder
                .fromPath("/agenda/actividadesByprofesionalesRegionalHorario")
                .queryParam("fechaTurno", fechaTurno)
                .queryParam("idRegional", idRegional)
                .queryParam("idHorarioTurno", idHorarioTurno);

        Response<List<Actividad>> response = webClient.get()
                .uri(builderUrl.build().toUri())
                .exchange()
                .expectStatus().isOk()
                .expectBody(new ParameterizedTypeReference<Response<List<Actividad>>>(){})
                .returnResult().getResponseBody();

        Assertions.assertNotNull(response);
        Assertions.assertEquals(respuestaEsperada.getResult(), response.getResult());
    }
    @Test
    void consultarActividadesProfesionalesRegionalHorarioError(){


        Mockito.when(agendaRepositoryMock.consultarActividadesByProfesionalesRegionalHorarioTurno(
                        fechaTurno,idHorarioTurno,idRegional))
                .thenReturn(Flux.error(Exception::new));

        UriComponentsBuilder builderUrl = UriComponentsBuilder
                .fromPath("/agenda/actividadesByprofesionalesRegionalHorario")
                .queryParam("fechaTurno", fechaTurno)
                .queryParam("idRegional", idRegional)
                .queryParam("idHorarioTurno", idHorarioTurno);

        Response<?> response = webClient.get()
                .uri(builderUrl.build().toUri())
                .exchange()
                .expectBody(Response.class)
                .returnResult().getResponseBody();

        Assertions.assertNotNull(response);
        Assertions.assertEquals(Mensajes.PETICION_FALLIDA, response.getMessage());
        Assertions.assertEquals(StatusCode.STATUS_500, response.getStatus());

    }
    @Test
    void getDesplazamientoByIdCitaPartida(){
        List<Desplazamiento> desplazamientos = new ArrayList<>();
        desplazamientos.add(Desplazamiento.builder().idCitaPartida("88sdf49-4").build());

        Response<List<Desplazamiento>> respuestaEsperada = Response.<List<Desplazamiento>>builder()
                .result(desplazamientos)
                .build();

        Mockito.when(agendaRepositoryMock.consultarDesplazamientoRegional(
                        fechaTurno,idHorarioTurno,idRegional))
                .thenReturn(Flux.fromIterable(desplazamientos));

        UriComponentsBuilder builderUrl = UriComponentsBuilder
                .fromPath("/agenda/desplazamientoVisita")
                .queryParam("fechaTurno", fechaTurno)
                .queryParam("idRegional", idRegional)
                .queryParam("idHorarioTurno", idHorarioTurno);

        Response<List<Desplazamiento>> response = webClient.get()
                .uri(builderUrl.build().toUri())
                .exchange()
                .expectStatus().isOk()
                .expectBody(new ParameterizedTypeReference<Response<List<Desplazamiento>>>(){})
                .returnResult().getResponseBody();

        Assertions.assertNotNull(response);
        Assertions.assertEquals(respuestaEsperada.getResult(), response.getResult());
    }
    @Test
    void getDesplazamientoByIdCitaPartidaError(){
        Mockito.when(agendaRepositoryMock.consultarDesplazamientoRegional(
                        fechaTurno,idHorarioTurno,idRegional))
                .thenReturn(Flux.error(Exception::new));

        UriComponentsBuilder builderUrl = UriComponentsBuilder
                .fromPath("/agenda/desplazamientoVisita")
                .queryParam("fechaTurno", fechaTurno)
                .queryParam("idRegional", idRegional)
                .queryParam("idHorarioTurno", idHorarioTurno);

        Response<?> response = webClient.get()
                .uri(builderUrl.build().toUri())
                .exchange()
                .expectBody(Response.class)
                .returnResult().getResponseBody();

        Assertions.assertNotNull(response);
        Assertions.assertEquals(Mensajes.PETICION_FALLIDA, response.getMessage());
        Assertions.assertEquals(StatusCode.STATUS_500, response.getStatus());
    }
    @Test
    void getCitasByTurnoRegional(){
        List<Cita> citas = new ArrayList<>();
        citas.add(Cita.builder().idCita("88sdf49-4").build());

        Response<List<Cita>> respuestaEsperada = Response.<List<Cita>>builder()
                .result(citas)
                .build();

        Mockito.when(agendaRepositoryMock.consultarCitasByTurnoRegional(
                        fechaTurno,idHorarioTurno,idRegional))
                .thenReturn(Flux.fromIterable(citas));

        UriComponentsBuilder builderUrl = UriComponentsBuilder
                .fromPath("/agenda/citas")
                .queryParam("fechaTurno", fechaTurno)
                .queryParam("idRegional", idRegional)
                .queryParam("idHorarioTurno", idHorarioTurno);

        Response<List<Cita>> response = webClient.get()
                .uri(builderUrl.build().toUri())
                .exchange()
                .expectStatus().isOk()
                .expectBody(new ParameterizedTypeReference<Response<List<Cita>>>(){})
                .returnResult().getResponseBody();

        Assertions.assertNotNull(response);
        Assertions.assertEquals(respuestaEsperada.getResult(), response.getResult());
    }
    @Test
    void getCitasByTurnoRegionalError(){
        Mockito.when(agendaRepositoryMock.consultarCitasByTurnoRegional(
                        fechaTurno,idHorarioTurno,idRegional))
                .thenReturn(Flux.error(Exception::new));

        UriComponentsBuilder builderUrl = UriComponentsBuilder
                .fromPath("/agenda/citas")
                .queryParam("fechaTurno", fechaTurno)
                .queryParam("idRegional", idRegional)
                .queryParam("idHorarioTurno", idHorarioTurno);

        Response<?> response = webClient.get()
                .uri(builderUrl.build().toUri())
                .exchange()
                .expectBody(Response.class)
                .returnResult().getResponseBody();

        Assertions.assertNotNull(response);
        Assertions.assertEquals(Mensajes.PETICION_FALLIDA, response.getMessage());
        Assertions.assertEquals(StatusCode.STATUS_500, response.getStatus());
    }
    @Test
    void reprogramarCita(){
        Cita cita = Cita.builder()
                .idCita(idCita)
                .idProfesional(idProfesional)
                .fechaProgramada(fechaProgramada)
                .idHorarioTurno(idHorarioTurno)
                .idRegional(idRegional)
                .build();

        Mockito.when(agendaRepositoryMock.reprogramarCitaFromProfesional(
                        fechaProgramada,idCita,idProfesional,fechaTurno,idHorarioTurno,idRegional))
                .thenReturn(Mono.just(Boolean.TRUE));


        Response<Boolean> response = webClient.put()
                .uri("/agenda/reprogramarCita")
                .bodyValue(cita)
                .exchange()
                .expectStatus().isOk()
                .expectBody(new ParameterizedTypeReference<Response<Boolean>>(){})
                .returnResult().getResponseBody();
        Assertions.assertNotNull(response);
        Assertions.assertTrue( response.getResult());
    }
    @Test
    void reprogramarCitaError(){
        Cita cita = Cita.builder()
                .idCita(idCita)
                .idProfesional(idProfesional)
                .fechaProgramada(fechaProgramada)
                .idHorarioTurno(idHorarioTurno)
                .idRegional(idRegional)
                .build();
        Mockito.when(agendaRepositoryMock.reprogramarCitaFromProfesional(
                        fechaProgramada,idCita,idProfesional,fechaTurno,idHorarioTurno,idRegional))
                .thenReturn(Mono.error(Exception::new));


        Response<?> response = webClient.put()
                .uri("/agenda/reprogramarCita")
                .bodyValue(cita)
                .exchange()
                .expectBody(Response.class)
                .returnResult().getResponseBody();

        Assertions.assertNotNull(response);
        Assertions.assertEquals(Mensajes.NO_REPROGRAMO_HORA_CITA, response.getDetail());
        Assertions.assertEquals(StatusCode.STATUS_500, response.getStatus());
    }
    @Test
    void confirmarCita(){

        Mockito.when(gestionEstadosCitasRepositoryMock.confirmarCita(
                        idCita))
                .thenReturn(Mono.just(Boolean.TRUE));

        UriComponentsBuilder builderUrl = UriComponentsBuilder
                .fromPath("/agenda/confirmarCita")
                .queryParam("idCita", idCita);

        Response<Boolean> response = webClient.put()
                .uri(builderUrl.build().toUri())
                .exchange()
                .expectStatus().isOk()
                .expectBody(new ParameterizedTypeReference<Response<Boolean>>(){})
                .returnResult().getResponseBody();

        Assertions.assertNotNull(response);
        Assertions.assertTrue( response.getResult());
    }
    @Test
    void confirmarCitaError(){
        Mockito.when(gestionEstadosCitasRepositoryMock.confirmarCita(
                        idCita))
                .thenReturn(Mono.error(Exception::new));

        UriComponentsBuilder builderUrl = UriComponentsBuilder
                .fromPath("/agenda/confirmarCita")
                .queryParam("idCita", idCita);

        Response<?> response = webClient.put()
                .uri(builderUrl.build().toUri())
                .exchange()
                .expectBody(Response.class)
                .returnResult().getResponseBody();

        Assertions.assertNotNull(response);
        Assertions.assertEquals(Mensajes.PETICION_FALLIDA, response.getDetail());
        Assertions.assertEquals(StatusCode.STATUS_500, response.getStatus());
    }
    @Test
    void iniciarAtencionCita(){

        Mockito.when(gestionEstadosCitasRepositoryMock.iniciarAtencionCita(
                        idCita))
                .thenReturn(Mono.just(Boolean.TRUE));

        UriComponentsBuilder builderUrl = UriComponentsBuilder
                .fromPath("/agenda/iniciarAtencionCita")
                .queryParam("idCita", idCita);

        Response<Boolean> response = webClient.put()
                .uri(builderUrl.build().toUri())
                .exchange()
                .expectStatus().isOk()
                .expectBody(new ParameterizedTypeReference<Response<Boolean>>(){})
                .returnResult().getResponseBody();

        Assertions.assertNotNull(response);
        Assertions.assertTrue( response.getResult());
    }
    @Test
    void iniciarAtencionCitaError(){

        Mockito.when(gestionEstadosCitasRepositoryMock.iniciarAtencionCita(
                        idCita))
                .thenReturn(Mono.error(Exception::new));

        UriComponentsBuilder builderUrl = UriComponentsBuilder
                .fromPath("/agenda/iniciarAtencionCita")
                .queryParam("idCita", idCita);

        Response<?> response = webClient.put()
                .uri(builderUrl.build().toUri())
                .exchange()
                .expectBody(Response.class)
                .returnResult().getResponseBody();

        Assertions.assertNotNull(response);
        Assertions.assertEquals(Mensajes.PETICION_FALLIDA, response.getDetail());
        Assertions.assertEquals(StatusCode.STATUS_500, response.getStatus());
    }
    @Test
    void finalizarAtencionCita(){

        Mockito.when(gestionEstadosCitasRepositoryMock.finalizarAtencionCita(
                        idCita))
                .thenReturn(Mono.just(Boolean.TRUE));

        UriComponentsBuilder builderUrl = UriComponentsBuilder
                .fromPath("/agenda/finalizarAtencionCita")
                .queryParam("idCita", idCita);

        Response<Boolean> response = webClient.put()
                .uri(builderUrl.build().toUri())
                .exchange()
                .expectStatus().isOk()
                .expectBody(new ParameterizedTypeReference<Response<Boolean>>(){})
                .returnResult().getResponseBody();

        Assertions.assertNotNull(response);
        Assertions.assertTrue( response.getResult());
    }
    @Test
    void finalizarAtencionCitaError(){
        Mockito.when(gestionEstadosCitasRepositoryMock.finalizarAtencionCita(
                        idCita))
                .thenReturn(Mono.error(Exception::new));

        UriComponentsBuilder builderUrl = UriComponentsBuilder
                .fromPath("/agenda/finalizarAtencionCita")
                .queryParam("idCita", idCita);

        Response<?> response = webClient.put()
                .uri(builderUrl.build().toUri())
                .exchange()
                .expectBody(Response.class)
                .returnResult().getResponseBody();

        Assertions.assertNotNull(response);
        Assertions.assertEquals(Mensajes.PETICION_FALLIDA, response.getDetail());
        Assertions.assertEquals(StatusCode.STATUS_500, response.getStatus());

    }
    @Test
    void asignarProfesionalCita(){
        Cita cita = Cita.builder()
                .idCita(idCita)
                .idProfesional(idProfesional)
                .fechaProgramada(fechaProgramada)
                .idHorarioTurno(idHorarioTurno)
                .idRegional(idRegional)
                .build();
        Mockito.when(gestionEstadosCitasRepositoryMock.agendarToProfesional(
                        idCita,idProfesional,fechaProgramada,idHorarioTurno,idRegional))
                .thenReturn(Mono.just(Boolean.TRUE));

        Response<Boolean> response = webClient.put()
                .uri("/agenda/asignarProfesionalCita")
                .bodyValue(cita)
                .exchange()
                .expectStatus().isOk()
                .expectBody(new ParameterizedTypeReference<Response<Boolean>>(){})
                .returnResult().getResponseBody();

        Assertions.assertNotNull(response);
        Assertions.assertTrue( response.getResult());
    }
    @Test
    void asignarProfesionalCitaError(){

        Cita cita = Cita.builder()
                .idCita(idCita)
                .idProfesional(idProfesional)
                .fechaProgramada(fechaProgramada)
                .idHorarioTurno(idHorarioTurno)
                .idRegional(idRegional)
                .build();

        Mockito.when(gestionEstadosCitasRepositoryMock.agendarToProfesional(
                        idCita,idProfesional,fechaProgramada,idHorarioTurno,idRegional))
                .thenReturn(Mono.error(Exception::new));

        Response<?> response = webClient.put()
                .uri("/agenda/asignarProfesionalCita")
                .bodyValue(cita)
                .exchange()
                .expectBody(Response.class)
                .returnResult().getResponseBody();

        Assertions.assertNotNull(response);
        Assertions.assertEquals(Mensajes.NO_ASIGNO_PROFESIONAL_CITA, response.getDetail());
        Assertions.assertEquals(StatusCode.STATUS_500, response.getStatus());
    }
    @Test
    void desasignarProfesionalCita(){

        Mockito.when(gestionEstadosCitasRepositoryMock.desagendarToProfesional(
                        idCita,idProfesional,fechaTurno,idHorarioTurno,idRegional))
                .thenReturn(Mono.just(Boolean.TRUE));

        UriComponentsBuilder builderUrl = UriComponentsBuilder
                .fromPath("/agenda/desasignarProfesionalCita")
                .queryParam("idCita", idCita)
                .queryParam("idProfesional", idProfesional)
                .queryParam("fechaTurno", fechaTurno)
                .queryParam("idHorarioTurno", idHorarioTurno)
                .queryParam("idRegional", idRegional);

        Response<Boolean> response = webClient.put()
                .uri(builderUrl.build().toUri())
                .exchange()
                .expectStatus().isOk()
                .expectBody(new ParameterizedTypeReference<Response<Boolean>>(){})
                .returnResult().getResponseBody();

        Assertions.assertNotNull(response);
        Assertions.assertTrue( response.getResult());
    }
    @Test
    void desasignarProfesionalCitaError(){
        Mockito.when(gestionEstadosCitasRepositoryMock.desagendarToProfesional(
                        idCita,idProfesional,fechaTurno,idHorarioTurno,idRegional))
                .thenReturn(Mono.error(Exception::new));

        UriComponentsBuilder builderUrl = UriComponentsBuilder
                .fromPath("/agenda/desasignarProfesionalCita")
                .queryParam("idCita", idCita)
                .queryParam("idProfesional", idProfesional)
                .queryParam("fechaTurno", fechaTurno)
                .queryParam("idHorarioTurno", idHorarioTurno)
                .queryParam("idRegional", idRegional);

        Response<?> response = webClient.put()
                .uri(builderUrl.build().toUri())
                .exchange()
                .expectBody(Response.class)
                .returnResult().getResponseBody();

        Assertions.assertNotNull(response);
        Assertions.assertEquals(Mensajes.PETICION_FALLIDA, response.getMessage());
        Assertions.assertEquals(StatusCode.STATUS_500, response.getStatus());
    }
    @Test
    void consultarTratamientosByCita(){
        List<Tratamiento> tratamientos = new ArrayList<>();
        tratamientos.add(Tratamiento.builder().build());

        Response<List<Tratamiento>> respuestaEsperada = Response.<List<Tratamiento>>builder()
                .result(tratamientos)
                .build();

        Mockito.when(agendaRepositoryMock.consultarTratamientoByCitas(idCita))
                .thenReturn(Flux.fromIterable(tratamientos));

        UriComponentsBuilder builderUrl = UriComponentsBuilder.fromPath("/agenda/tratamientos")
                .queryParam("idCita", idCita);


        Response<List<Tratamiento>> response = webClient.get()
                .uri(builderUrl.build().toUri())
                .exchange()
                .expectStatus().isOk()
                .expectBody(new ParameterizedTypeReference<Response<List<Tratamiento>>>(){})
                .returnResult().getResponseBody();

        Assertions.assertNotNull(response);
        Assertions.assertEquals(respuestaEsperada.getResult(), response.getResult());
    }
    @Test
    void consultarTratamientosByCitaError(){
        Mockito.when(agendaRepositoryMock.consultarTratamientoByCitas(idCita))
                .thenReturn(Flux.error(Exception::new));

        UriComponentsBuilder builderUrl = UriComponentsBuilder.fromPath("/agenda/tratamientos")
                .queryParam("idCita", idCita);


        Response<?> response = webClient.get()
                .uri(builderUrl.build().toUri())
                .exchange()
                .expectBody(Response.class)
                .returnResult().getResponseBody();

        Assertions.assertNotNull(response);
        Assertions.assertEquals(Mensajes.PETICION_FALLIDA, response.getMessage());
        Assertions.assertEquals(StatusCode.STATUS_500, response.getStatus());
    }
    @Test
    void consultarProcedimientosByIdCita(){
        Procedimientos procedimientos = Procedimientos.builder().build();


        Response<Procedimientos> respuestaEsperada = Response.<Procedimientos>builder()
                .result(procedimientos)
                .build();

        Mockito.when(agendaRepositoryMock.consultarProcedimientosByIdCita(idCita))
                .thenReturn(Mono.just(procedimientos));

        UriComponentsBuilder builderUrl = UriComponentsBuilder.fromPath("/agenda/procedimientos")
                .queryParam("idCita", idCita);


        Response<Procedimientos> response = webClient.get()
                .uri(builderUrl.build().toUri())
                .exchange()
                .expectStatus().isOk()
                .expectBody(new ParameterizedTypeReference<Response<Procedimientos>>(){})
                .returnResult().getResponseBody();

        Assertions.assertNotNull(response);
        Assertions.assertEquals(respuestaEsperada.getResult(), response.getResult());
    }
    @Test
    void consultarProcedimientosByIdCitaError(){
        Mockito.when(agendaRepositoryMock.consultarProcedimientosByIdCita(idCita))
                .thenReturn(Mono.error(Exception::new));

        UriComponentsBuilder builderUrl = UriComponentsBuilder.fromPath("/agenda/procedimientos")
                .queryParam("idCita", idCita);


        Response<?> response = webClient.get()
                .uri(builderUrl.build().toUri())
                .exchange()
                .expectBody(Response.class)
                .returnResult().getResponseBody();

        Assertions.assertNotNull(response);
        Assertions.assertEquals(Mensajes.PETICION_FALLIDA, response.getMessage());
        Assertions.assertEquals(StatusCode.STATUS_500, response.getStatus());
    }
}
