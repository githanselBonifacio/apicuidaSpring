package co.com.sura.agenda;

import co.com.sura.agenda.entity.Actividad;
import co.com.sura.agenda.entity.Cita;
import co.com.sura.agenda.entity.Tarea;
import co.com.sura.agenda.gateway.ProcedimientosCitaRepository;
import co.com.sura.genericos.EstadosCita;
import co.com.sura.moviles.entity.Desplazamiento;
import co.com.sura.personal.entity.TurnoProfesional;
import co.com.sura.postgres.agenda.adapter.AgendaRepositoryAdapter;
import co.com.sura.postgres.agenda.adapter.AgendamientoAutomaticoAdapter;
import co.com.sura.postgres.agenda.data.CitaData;
import co.com.sura.postgres.agenda.repository.CitaRepository;
import co.com.sura.postgres.maestros.data.HorarioTurnoData;
import co.com.sura.postgres.maestros.repository.HorarioTurnoRepository;
import co.com.sura.postgres.moviles.data.DesplazamientoData;
import co.com.sura.postgres.moviles.data.DesplazamientoRepository;
import co.com.sura.postgres.personal.data.ProfesionalData;
import co.com.sura.postgres.personal.data.TurnoProfesionalesData;
import co.com.sura.postgres.personal.repository.TurnoProfesionalesRepository;
import co.com.sura.postgres.remision.data.tratamientos.TratamientoData;
import co.com.sura.postgres.remision.repository.tratamientos.TratamientoRepository;
import co.com.sura.remision.entity.datosremision.Tratamiento;
import co.com.sura.remision.entity.procedimientos.Procedimientos;
import org.junit.jupiter.api.BeforeEach;
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
import java.time.LocalTime;
import java.util.ArrayList;
import java.util.List;

@ExtendWith(MockitoExtension.class)
class AgendaRepositoryAdapterTest {
    private final LocalDate fechaTurno = LocalDate.now();
    private final LocalDateTime fechaProgramada = LocalDateTime.of(LocalDate.now(), LocalTime.of(8, 10));
    private final String idProfesional = "454654654";
    private final String idRemision = "asfasf4as";
    private final String idCita = "asfasf4as-1";
    private final String idRegional = "427";
    private final Integer idHorarioTurno = 1;
    private CitaData citaData;
    @Mock
    private TurnoProfesionalesRepository turnoProfesionalesRepositoryMock;
    @Mock
    private CitaRepository citaRepositoryMock;
    @Mock
    private DesplazamientoRepository desplazamientoRepositoryMock;
    @Mock
    private TratamientoRepository tratamientoRepositoryMock;
    @Mock
    private ProcedimientosCitaRepository procedimientosCitaRepositoryMock;
    @Mock
    private HorarioTurnoRepository horarioTurnoRepositoryMock;

    @Mock
    private AgendamientoAutomaticoAdapter agendamientoAutomaticoAdapterMock;


    @InjectMocks
    private AgendaRepositoryAdapter agendaRepositoryAdapter;

    @BeforeEach()
    void setUp(){
        this.citaData =  CitaData.builder()
                .idCita(idRemision + "-1")
                .idRemision(idRemision)
                .duracion(600)
                .idEstado(1)
                .holgura(600)
                .fechaInicio(fechaProgramada)
                .fechaProgramada(fechaProgramada)
                .especialidad("especialidad")
                .idRegional("427")
                .idHorarioTurno(idHorarioTurno)
                .idProfesional(idProfesional)
                .idConductor(null)
                .latitud(0.0)
                .longitud(0.0)
                .build();
    }

    @Test
    void asignarProfesionalTurno() {
        TurnoProfesional turnoProfesional = TurnoProfesional.builder()
                .fechaTurno(fechaTurno)
                .build();
        TurnoProfesionalesData turnoProfesionalesData = TurnoProfesionalesData.builder()
                .fechaTurno(fechaTurno)
                .build();

        Mockito.when(turnoProfesionalesRepositoryMock.save(turnoProfesionalesData))
                .thenReturn(Mono.just(turnoProfesionalesData));

        Mono<Boolean> response = agendaRepositoryAdapter.asignarProfesionalTurno(turnoProfesional);

        StepVerifier.create(response)
                .expectNext(Boolean.TRUE)
                .expectComplete()
                .verify();
    }

    @Test
    void desasignarProfesionalTurno() {
        TurnoProfesional turnoProfesional = TurnoProfesional.builder()
                .idProfesional(idProfesional)
                .fechaTurno(fechaTurno)
                .idRegional(idRegional)
                .idHorarioTurno(idHorarioTurno)
                .build();

        List<CitaData> citasData = new ArrayList<>();
        citasData.add(citaData);

        //mocks
        Mockito.when(citaRepositoryMock.findAllByTurnoProfesional(fechaTurno, idProfesional))
                .thenReturn(Flux.fromIterable(citasData));

        Mockito.when(citaRepositoryMock.desagendarAllFromIdProfesional(
                        fechaTurno, idHorarioTurno, idProfesional, EstadosCita.SIN_AGENDAR.getEstado()))
                .thenReturn(Mono.empty());


        Mockito.when(turnoProfesionalesRepositoryMock.deleteByFechaTurnoIdHorarioProfesional(
                        fechaTurno, idHorarioTurno, idProfesional))
                .thenReturn(Mono.empty());

        Mockito.when(desplazamientoRepositoryMock.deleteByFechaTurnoProfesional(
                        fechaTurno, idHorarioTurno, idRegional, idProfesional))
                .thenReturn(Mono.empty());

        Mono<Boolean> response = agendaRepositoryAdapter.desasignarProfesionalTurno(turnoProfesional);

        StepVerifier.create(response)
                .expectNext(Boolean.TRUE)
                .expectComplete()
                .verify();
    }

    @Test
    void consultarTareasTurnoByProfesional() {
        List<CitaData> citasData = new ArrayList<>();
        citasData.add(this.citaData);

        ProfesionalData profesionalData = ProfesionalData.builder()
                .idRegional(idRegional)
                .numeroIdentificacion(idProfesional)
                .build();

        DesplazamientoData desplazamientoData = DesplazamientoData.builder()
                .idCitaPartida(idRemision + "-1")
                .idCitaDestino(idRemision + "-2")
                .fechaProgramada(fechaProgramada)
                .tipo("VISITA")
                .duracion(600)
                .holgura(600)
                .idHorarioTurno(idHorarioTurno)
                .idRegional(idRegional)
                .build();
        List<DesplazamientoData> desplazamientoDataList = new ArrayList<>();
        desplazamientoDataList.add(desplazamientoData);

        Tarea tarea = Tarea.builder()
                .id(idRemision + "-1")
                .latitud(0.0)
                .longitud(0.0)
                .duracion(600)
                .holgura(600)
                .idEstado(1)
                .tipo("VISITA")
                .fechaProgramada(fechaProgramada)
                .fechaInicio(fechaProgramada)
                .build();

        Mockito.when(citaRepositoryMock.findAllByTurnoRegionalProfesional(
                        fechaTurno, idHorarioTurno, idRegional, idProfesional, EstadosCita.CANCELADA.getEstado()))
                .thenReturn(Flux.fromIterable(citasData));

        Mockito.when(desplazamientoRepositoryMock.findAllByTurnoProfesional(
                        fechaTurno, idHorarioTurno, idRegional, idProfesional))
                .thenReturn(Flux.fromIterable(desplazamientoDataList));

        Flux<Tarea> response = agendaRepositoryAdapter.consultarTareasTurnoByProfesional(
                profesionalData, fechaTurno, idHorarioTurno, idRegional);

        StepVerifier.create(response)
                .expectNext(tarea)
                .expectComplete();
    }

    @Test
    void consultarActividadesByProfesionalesRegionalHorarioTurno() {

        List<ProfesionalData> profesionalDataList = new ArrayList<>();
        ProfesionalData profesionalData = ProfesionalData.builder()
                .numeroIdentificacion(idProfesional)
                .idRegional(idRegional)
                .build();
        profesionalDataList.add(profesionalData);

        List<CitaData> citas = new ArrayList<>();
        citas.add(this.citaData);

        List<DesplazamientoData> desplazamientoDataList = new ArrayList<>();
        DesplazamientoData desplazamientoData = DesplazamientoData.builder()
                .idCitaPartida(idRemision+"-1")
                .idCitaDestino(idRemision+"-2")
                .fechaProgramada(fechaProgramada)
                .tipo("VISITA")
                .holgura(600)
                .duracion(600)
                .build();

        desplazamientoDataList.add(desplazamientoData);

        Mockito.when(citaRepositoryMock.findAllByTurnoRegionalProfesional(
                fechaTurno,idHorarioTurno,idRegional,idProfesional,EstadosCita.CANCELADA.getEstado()))
                .thenReturn(Flux.fromIterable(citas));

        Mockito.when(desplazamientoRepositoryMock.findAllByTurnoProfesional(
                        fechaTurno,idHorarioTurno,idRegional,idProfesional))
                .thenReturn(Flux.fromIterable(desplazamientoDataList));

        Mockito.when(turnoProfesionalesRepositoryMock.findTurnoProfesionalByRegionalHorario(
                        fechaTurno, idHorarioTurno, idRegional))
                .thenReturn(Flux.fromIterable(profesionalDataList));

        List<Tarea> tareas = new ArrayList<>();
        Tarea tarea = Tarea.builder()
                .id(idRemision + "-1")
                .latitud(0.0)
                .longitud(0.0)
                .duracion(600)
                .holgura(600)
                .idEstado(1)
                .tipo("VISITA")
                .fechaProgramada(fechaProgramada)
                .fechaInicio(fechaProgramada)
                .build();

        Tarea tareaDesplazamiento= Tarea.builder()
                .id(idRemision + "-1 -> "+idRemision+"-2")
                .duracion(600)
                .holgura(600)
                .tipo("VISITA")
                .fechaProgramada(fechaProgramada)
                .fechaInicio(fechaProgramada)
                .build();
        tareas.add(tarea);
        tareas.add(tareaDesplazamiento);
        Actividad actividad = Actividad.builder()
                .responsable("null null")
                .numeroIdentificacion(idProfesional)
                .tareas(tareas)
                .build();

        Flux<Actividad> actividades = agendaRepositoryAdapter.consultarActividadesByProfesionalesRegionalHorarioTurno(
                fechaTurno, idHorarioTurno, idRegional);

        StepVerifier.create(actividades)
                .expectNext(actividad)
                .verifyComplete();
    }

    @Test
    void consultarCitasByTurnoRegional() {
        List<Cita> citas = new ArrayList<>();
        Cita cita = Cita.builder()
                .idCita(idRemision + "-1")
                .idRemision(idRemision)
                .build();

        citas.add(cita);
        //mocks
        Mockito.when(citaRepositoryMock.findAllByTurnoRegionalHorario(
                        fechaTurno, idHorarioTurno, idRegional, EstadosCita.CANCELADA.getEstado()))
                .thenReturn(Flux.fromIterable(citas));

        Flux<Cita> response = agendaRepositoryAdapter.consultarCitasByTurnoRegional(
                fechaTurno, idHorarioTurno, idRegional);

        StepVerifier.create(response)
                .expectNext(cita)
                .expectComplete()
                .verify();
    }

    @Test
    void consultarDesplazamientoRegional() {
        List<DesplazamientoData> desplazamientosData = new ArrayList<>();
        DesplazamientoData desplazamientoData = DesplazamientoData.builder()
                .idCitaPartida(idRemision + "-1")
                .build();

        desplazamientosData.add(desplazamientoData);
        //mocks
        Mockito.when(desplazamientoRepositoryMock.findByFechaProgramada(
                        fechaTurno, idRegional, idHorarioTurno))
                .thenReturn(Flux.fromIterable(desplazamientosData));

        Flux<Desplazamiento> response = agendaRepositoryAdapter.consultarDesplazamientoRegional(
                fechaTurno, idHorarioTurno, idRegional);

        StepVerifier.create(response)
                .expectNextCount(1)
                .expectComplete();
    }

    @Test
    void consultarTratamientoByCitas() {
        List<TratamientoData> tratamientosData = new ArrayList<>();
        TratamientoData tratamientoData = TratamientoData.builder()
                .build();

        tratamientosData.add(tratamientoData);
        //mocks
        Mockito.when(tratamientoRepositoryMock.findByIdCita(idCita))
                .thenReturn(Flux.fromIterable(tratamientosData));

        Flux<Tratamiento> response = agendaRepositoryAdapter.consultarTratamientoByCitas(idCita);

        StepVerifier.create(response)
                .expectNextCount(1)
                .expectComplete();
    }

    @Test
    void consultarProcedimientosByIdCita() {
        Procedimientos procedimientos = Procedimientos.builder().build();

        //mocks
        Mockito.when(procedimientosCitaRepositoryMock.consultarProcedimientosByIdCita(idCita))
                .thenReturn(Mono.just(procedimientos));

        Mono<Procedimientos> response = agendaRepositoryAdapter.consultarProcedimientosByIdCita(idCita);

        StepVerifier.create(response)
                .expectNextCount(1)
                .expectComplete();
    }

    @Test
    void reprogramarCitaFromProfesional() {

        Mockito.when(citaRepositoryMock.findById(idCita))
                .thenReturn(Mono.just(this.citaData));

        Mono<Boolean> response = agendaRepositoryAdapter.reprogramarCitaFromProfesional(
                fechaProgramada, idCita, idProfesional, fechaTurno, idHorarioTurno, idRegional);

        StepVerifier.create(response)
                .expectNext(Boolean.TRUE);


    }

    @Test
    void validarDisponibilidadFechaCita() {


        Mockito.when(citaRepositoryMock.findMasCercanaAnterior(
                fechaProgramada,idCita,idHorarioTurno,idRegional,idProfesional))
                .thenReturn(Mono.just(this.citaData));

        Mockito.when(citaRepositoryMock.findMasCercanaPosterior(
                         fechaProgramada.plusMinutes(10)
                        ,idCita,idHorarioTurno,idRegional,idProfesional))
                .thenReturn(Mono.just(this.citaData));

        Mockito.when(desplazamientoRepositoryMock.findByIdCitaPartida(idCita))
                .thenReturn(Mono.just(DesplazamientoData.builder().duracion(600).build()));

        Mockito.when(desplazamientoRepositoryMock.findBySede(
                fechaProgramada,idRegional,idProfesional,idHorarioTurno))
                .thenReturn(Mono.just(DesplazamientoData.builder().duracion(600).build()));

        Mono<Boolean> response = agendaRepositoryAdapter.validarDisponibilidadFechaCita(
                this.citaData, fechaProgramada, idProfesional);

        StepVerifier.create(response)
                .expectNext(Boolean.FALSE)
                .verifyComplete();
    }
    @Test
    void validarAgendamientoCitaEnHorarioTurno(){

        Mockito.when(horarioTurnoRepositoryMock.findById(idHorarioTurno))
                .thenReturn(Mono.just(HorarioTurnoData.builder()
                                .id(idHorarioTurno)
                                .horaInicio(LocalTime.of(6,0))
                                .horaFin(LocalTime.of(13,59))
                                .esHorarioBase(Boolean.TRUE)
                        .build()));

        Mono<Boolean> response = agendaRepositoryAdapter.validarAgendamientoCitaEnHorarioTurno(
                this.citaData, fechaProgramada);

        StepVerifier.create(response)
                .expectNext(Boolean.TRUE)
                .verifyComplete();
    }
}
