package co.com.sura.agenda;

import co.com.sura.agenda.entity.Actividad;
import co.com.sura.agenda.entity.Cita;
import co.com.sura.agenda.gateway.AgendaRepository;
import co.com.sura.agenda.gateway.AgendamientoAutomaticoRepository;
import co.com.sura.agenda.gateway.GestionEstadosCitasRepository;
import co.com.sura.moviles.entity.Desplazamiento;
import co.com.sura.personal.entity.Profesional;
import co.com.sura.personal.entity.TurnoProfesional;
import co.com.sura.personal.gateway.PersonalCrudRepository;
import co.com.sura.remision.entity.datosremision.Tratamiento;
import co.com.sura.remision.entity.procedimientos.Procedimientos;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import reactor.test.StepVerifier;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;

@ExtendWith(MockitoExtension.class)
 class AgendaUserCaseTest {
    LocalDate fechaTurno = LocalDate.of(2023,7,7);
    LocalDateTime fechaTurnoProgramada = LocalDateTime.of(2023,7,7,8,0);
    String idRegional = "427";
    Integer idHorarioTurno = 1;
    String idProfesional = "989898";
    String idCita = "af456asdf-1";
    @Mock
    private PersonalCrudRepository personalCrudRepositoryMock;
    @Mock
    private AgendaRepository agendaRepositoryMock;
    @Mock
    private AgendamientoAutomaticoRepository agendamientoAutomaticoRepositoryMock;
    @Mock
    private GestionEstadosCitasRepository gestionEstadosCitasRepositoryMock;
    @InjectMocks
    private AgendaUseCase agendaUseCaseMock;

    @Test
    void consultarProfesionalesByTurnoRegional(){
        List<Profesional> profesionales = new ArrayList<>();
        profesionales.add(Profesional.builder().build());
        profesionales.add(Profesional.builder().build());

        Mockito.when(personalCrudRepositoryMock.consultarProfesionalByTurnoRegional(fechaTurno,idRegional))
                .thenReturn(Flux.fromIterable(profesionales));

        Flux<Profesional> consultaProfesional = agendaUseCaseMock
                .consultarProfesionalesByTurnoRegional(fechaTurno,idRegional);

        StepVerifier.create(consultaProfesional)
                .expectNextCount(2)
                .expectComplete()
                .verify();
    }

    @Test
    void consultarProfesionalesFromTurnoRegional(){
        List<Profesional> profesionales = new ArrayList<>();
        profesionales.add(Profesional.builder().build());
        profesionales.add(Profesional.builder().build());

        Mockito.when(personalCrudRepositoryMock.consultarProfesionalFromTurnoRegional(fechaTurno,idRegional,idHorarioTurno))
                .thenReturn(Flux.fromIterable(profesionales));

        Flux<Profesional> consultaProfesional = agendaUseCaseMock
                .consultarProfesionalesFromTurnoRegional(fechaTurno,idRegional,idHorarioTurno);

        StepVerifier.create(consultaProfesional)
                .expectNextCount(2)
                .expectComplete()
                .verify();
    }

    @Test
    void asignarProfesionalTurno(){
        TurnoProfesional turnoProfesional = TurnoProfesional.builder().build();

        Mockito.when(agendaRepositoryMock.asignarProfesionalTurno(turnoProfesional)).thenReturn(Mono.just(true));

        Mono<Boolean> esAsignadoTurno= agendaUseCaseMock.asignarProfesionalTurno(turnoProfesional);

        StepVerifier.create(esAsignadoTurno)
                .expectNext(Boolean.TRUE)
                .expectComplete()
                .verify();
    }
    @Test
    void desasignarProfesionalTurno(){
        TurnoProfesional turnoProfesional = TurnoProfesional.builder().build();

        Mockito.when(agendaRepositoryMock.desasignarProfesionalTurno(turnoProfesional))
                .thenReturn(Mono.just(Boolean.TRUE));

        Mono<Boolean> esDesasignadoTurno= agendaUseCaseMock.desasignarProfesionalTurno(turnoProfesional);

        StepVerifier.create(esDesasignadoTurno)
                .expectNext(Boolean.TRUE)
                .expectComplete()
                .verify();
    }

    @Test
    void desagendarTurnoCompleto(){
        Mockito.when(agendamientoAutomaticoRepositoryMock.desagendarTurnoCompleto(fechaTurno,idHorarioTurno,idRegional))
                .thenReturn(Mono.just(Boolean.TRUE));

        Mono<Boolean> esDesasignadoTurnoCompleto= agendaUseCaseMock.desagendarTurnoCompleto(
                fechaTurno,idHorarioTurno,idRegional
        );

        StepVerifier.create(esDesasignadoTurnoCompleto)
                .expectNext(Boolean.TRUE)
                .expectComplete()
                .verify();
    }

    @Test
    void consultarProfesionalesByRegional(){
        List<Profesional> profesionales = new ArrayList<>();
        profesionales.add(Profesional.builder().build());
        profesionales.add(Profesional.builder().build());

        Mockito.when(personalCrudRepositoryMock.consultarProfesionalesByIdRegional(idRegional))
                .thenReturn(Flux.fromIterable(profesionales));

        Flux<Profesional> consultaProfesional = agendaUseCaseMock
                .consultarProfesionalesByRegional(idRegional);

        StepVerifier.create(consultaProfesional)
                .expectNextCount(2)
                .expectComplete()
                .verify();
    }

    @Test
    void consultarActividadesProfesionalesRegionalHorario(){
        List<Actividad> actividad = new ArrayList<>();
        actividad.add(Actividad.builder().build());
        actividad.add(Actividad.builder().build());

        Mockito.when(agendaRepositoryMock.consultarActividadesByProfesionalesRegionalHorarioTurno(
                fechaTurno,idHorarioTurno,idRegional
                ))
                .thenReturn(Flux.fromIterable(actividad));

        Flux<Actividad> consultaActividades = agendaUseCaseMock
                .consultarActividadesProfesionalesRegionalHorario(  fechaTurno,idHorarioTurno,idRegional);

        StepVerifier.create(consultaActividades)
                .expectNextCount(2)
                .expectComplete()
                .verify();
    }

    @Test
    void autoagendarTurnoCompleto(){
        Mockito.when(agendamientoAutomaticoRepositoryMock.autoagendarTurnoCompleto(fechaTurno,idHorarioTurno,idRegional))
                .thenReturn(Mono.just(true));

        Mono<Boolean> esautoagendadoTurnoCompleto= agendaUseCaseMock.autoagendarTurnoCompleto(
                fechaTurno,idHorarioTurno,idRegional
        );

        StepVerifier.create(esautoagendadoTurnoCompleto)
                .expectNext(Boolean.TRUE)
                .expectComplete()
                .verify();
    }

    @Test
    void consultarDesplazamientoByIdCitaPartida(){
        List<Desplazamiento> desplazamientos = new ArrayList<>();
        desplazamientos.add(Desplazamiento.builder().build());
        desplazamientos.add(Desplazamiento.builder().build());

        Mockito.when(agendaRepositoryMock.consultarDesplazamientoRegional(
                        fechaTurno,idHorarioTurno,idRegional
                ))
                .thenReturn(Flux.fromIterable(desplazamientos));

        Flux<Desplazamiento> consultaDesplazamientos = agendaUseCaseMock
                .consultarDesplazamientoByTurnoRegional(  fechaTurno,idHorarioTurno,idRegional);

        StepVerifier.create(consultaDesplazamientos)
                .expectNextCount(2)
                .expectComplete()
                .verify();
    }

    @Test
    void consultarCitasByTurnoRegional(){
        List<Cita> citas = new ArrayList<>();
        citas.add(Cita.builder().build());
        citas.add(Cita.builder().build());

        Mockito.when(agendaRepositoryMock.consultarCitasByTurnoRegional(
                        fechaTurno,idHorarioTurno,idRegional
                ))
                .thenReturn(Flux.fromIterable(citas));

        Flux<Cita> consultaCitas = agendaUseCaseMock
                .consultarCitasByTurnoRegional(  fechaTurno,idHorarioTurno,idRegional);

        StepVerifier.create(consultaCitas)
                .expectNextCount(2)
                .expectComplete()
                .verify();
    }

    @Test
    void reprogramarCitaById(){
        Mockito.when(agendaRepositoryMock.reprogramarCitaFromProfesional(
                fechaTurnoProgramada,idCita,idProfesional,fechaTurno,idHorarioTurno,idRegional))
                .thenReturn(Mono.just(Boolean.TRUE));

        Mono<Boolean> seReprogramaCita= agendaUseCaseMock.reprogramarCitaById(
                fechaTurnoProgramada,idCita,idProfesional,fechaTurno,idHorarioTurno,idRegional
        );

        StepVerifier.create(seReprogramaCita)
                .expectNext(Boolean.TRUE)
                .expectComplete()
                .verify();
    }

    @Test
    void asignarProfesionaCita(){
        Mockito.when(gestionEstadosCitasRepositoryMock.agendarToProfesional(
                       idCita,idProfesional,fechaTurnoProgramada,idHorarioTurno,idRegional))
                .thenReturn(Mono.just(Boolean.TRUE));

        Mono<Boolean> seAsignoProfesional= agendaUseCaseMock.asignarProfesionaCita(
                idCita,idProfesional,fechaTurnoProgramada,idHorarioTurno,idRegional
        );

        StepVerifier.create(seAsignoProfesional)
                .expectNext(Boolean.TRUE)
                .expectComplete()
                .verify();
    }

    @Test
    void desasignarProfesionaCita(){
        Mockito.when(gestionEstadosCitasRepositoryMock.desagendarToProfesional(
                        idCita,idProfesional,fechaTurno,idHorarioTurno,idRegional))
                .thenReturn(Mono.just(Boolean.TRUE));

        Mono<Boolean> seDesasignoProfesional= agendaUseCaseMock.desasignarProfesionaCita(
                idCita,idProfesional,fechaTurno,idHorarioTurno,idRegional
        );

        StepVerifier.create(seDesasignoProfesional)
                .expectNext(Boolean.TRUE)
                .expectComplete()
                .verify();
    }

    @Test
    void confirmarCita(){
        Mockito.when(gestionEstadosCitasRepositoryMock.confirmarCita(idCita))
                .thenReturn(Mono.just(Boolean.TRUE));

        Mono<Boolean> seConfirmoCita= agendaUseCaseMock.confirmarCita(idCita);

        StepVerifier.create(seConfirmoCita)
                .expectNext(Boolean.TRUE)
                .expectComplete()
                .verify();
    }

    @Test
    void iniciarAtencionCita(){
        Mockito.when(gestionEstadosCitasRepositoryMock.iniciarAtencionCita(idCita))
                .thenReturn(Mono.just(Boolean.TRUE));

        Mono<Boolean> seInicioatencion= agendaUseCaseMock.iniciarAtencionCita(idCita);

        StepVerifier.create(seInicioatencion)
                .expectNext(Boolean.TRUE)
                .expectComplete()
                .verify();
    }

    @Test
    void finalizarAtencionCita(){
        Mockito.when(gestionEstadosCitasRepositoryMock.finalizarAtencionCita(idCita))
                .thenReturn(Mono.just(Boolean.TRUE));

        Mono<Boolean> seFinalizoAtencion= agendaUseCaseMock.finalizarAtencionCita(idCita);

        StepVerifier.create(seFinalizoAtencion)
                .expectNext(Boolean.TRUE)
                .expectComplete()
                .verify();
    }

    @Test
    void consultarProcedimietosByIdCita(){
       Procedimientos procedimientos = Procedimientos.builder().build();

        Mockito.when(agendaRepositoryMock.consultarProcedimientosByIdCita(idCita))
                .thenReturn(Mono.just(procedimientos));

        Mono<Procedimientos> consultaProcedimientos = agendaUseCaseMock.consultarProcedimietosByIdCita(idCita);

        StepVerifier.create(consultaProcedimientos)
                .expectNext(procedimientos)
                .expectComplete()
                .verify();
    }
    @Test
    void consultarTratamientosByCita(){
        List<Tratamiento> tratamientos = new ArrayList<>();
        tratamientos.add(Tratamiento.builder().build());
        tratamientos.add(Tratamiento.builder().build());

        Mockito.when(agendaRepositoryMock.consultarTratamientoByCitas(idCita))
                .thenReturn(Flux.fromIterable(tratamientos));

        Flux<Tratamiento> consultaTratamientos = agendaUseCaseMock.consultarTratamientosByCita(idCita);

        StepVerifier.create(consultaTratamientos)
                .expectNextCount(2)
                .expectComplete()
                .verify();
    }

}
