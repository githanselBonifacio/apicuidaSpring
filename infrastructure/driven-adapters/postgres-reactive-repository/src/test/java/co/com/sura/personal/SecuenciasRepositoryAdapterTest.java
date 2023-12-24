package co.com.sura.personal;

import co.com.sura.constantes.Mensajes;
import co.com.sura.genericos.ResultadoActualizacionTurno;
import co.com.sura.maestros.entity.HorarioTurno;
import co.com.sura.personal.entity.ProfesionalWithTurno;
import co.com.sura.personal.entity.SecuenciaTurno;
import co.com.sura.personal.entity.TurnoProfesional;
import co.com.sura.postgres.agenda.data.CitaData;
import co.com.sura.postgres.agenda.repository.CitaRepository;
import co.com.sura.postgres.maestros.data.HorarioTurnoData;
import co.com.sura.postgres.maestros.repository.HorarioTurnoRepository;
import co.com.sura.postgres.personal.adapter.SecuenciasHorarioAdapter;
import co.com.sura.postgres.personal.data.ItemSecuenciaTurnoData;
import co.com.sura.postgres.personal.data.ProfesionalData;
import co.com.sura.postgres.personal.data.TurnoProfesionalesData;
import co.com.sura.postgres.personal.repository.ItemSecuenciaTurnoRepository;
import co.com.sura.postgres.personal.repository.ProfesionalRepository;
import co.com.sura.postgres.personal.repository.TurnoProfesionalesRepository;
import co.com.sura.remision.dto.EliminarTurnoProfesionalRequest;
import co.com.sura.remision.entity.datosremision.ItemDiaTurno;
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
import java.time.LocalTime;
import java.util.ArrayList;
import java.util.List;

@ExtendWith(MockitoExtension.class)
class SecuenciasRepositoryAdapterTest {

    private final String idProfesional = "985802";
    private final String idRegional = "427";

    private final Integer idHorarioTurno = 1;
    private final LocalDate fechaTurno = LocalDate.now();
    private   ProfesionalWithTurno profesionalWithTurno;
    private ProfesionalData profesionalData;

    private TurnoProfesionalesData turnoProfesionalesData;
    private TurnoProfesional turnoProfesional;
    private CitaData citaData;
    private ItemSecuenciaTurnoData itemSecuenciaTurnoData;
    @Mock
    private  CitaRepository citaRepositoryMock;
    @Mock
    private  ProfesionalRepository profesionalRepositoryMock;
    @Mock
    private  TurnoProfesionalesRepository turnoProfesionalesRepositoryMock;
    @Mock
    private  ItemSecuenciaTurnoRepository secuenciaTurnoRepositoryMock;
    @Mock
    private  HorarioTurnoRepository horarioTurnoRepositoryMock;

    @InjectMocks
    private SecuenciasHorarioAdapter secuenciasHorarioAdapter;

    @BeforeEach
    void setUpData(){

        profesionalWithTurno = ProfesionalWithTurno
                .builder()
                .numeroIdentificacion(idProfesional)
                .idRegional(idRegional)
                .build();

        profesionalData = ProfesionalData
                .builder()
                .idRegional(idRegional)
                .numeroIdentificacion(idProfesional)
                .build();

        turnoProfesionalesData = TurnoProfesionalesData.builder()
                .idRegional(idRegional)
                .idHorarioTurno(idHorarioTurno)
                .idProfesional(idProfesional)
                .fechaTurno(fechaTurno)
                .build();

        turnoProfesional = TurnoProfesional
                .builder()
                .idRegional(idRegional)
                .idHorarioTurno(idHorarioTurno)
                .idProfesional(idProfesional)
                .fechaTurno(fechaTurno)
                .build();

        citaData = CitaData.builder()
                .idRegional(idRegional)
                .idHorarioTurno(idHorarioTurno)
                .idProfesional(idProfesional)
                .fechaProgramada(fechaTurno.atTime(LocalTime.of(8,30)))
                .fechaInicio(fechaTurno.atTime(LocalTime.of(8,30)))
                .build();

        itemSecuenciaTurnoData = ItemSecuenciaTurnoData.builder()
                .nombreSecuencia("ss0")
                .descripcion("s")
                .numeroDia(1)
                .nombreDia("Lunes")
                .idHorarioTurno(idHorarioTurno)
                .build();
    }

    @Test
    void consultarHorariosProfesionales(){

        List<ProfesionalData> profesionalesData = new ArrayList<>();
        profesionalesData.add(profesionalData);

        List<TurnoProfesionalesData> turnoProfesionales = new ArrayList<>();
        turnoProfesionales.add(turnoProfesionalesData);

        Mockito.when(profesionalRepositoryMock.findByIdRegional(idRegional))
                .thenReturn(Flux.fromIterable(profesionalesData));

        Mockito.when(turnoProfesionalesRepositoryMock.findTurnoProfesionalByFechaRegional(
                        fechaTurno.toString(),profesionalData.getNumeroIdentificacion(),idRegional))
                .thenReturn(Flux.fromIterable(turnoProfesionales));


        Flux<ProfesionalWithTurno> response = secuenciasHorarioAdapter.consultarHorariosProfesionales(
                fechaTurno.toString(),idRegional);

        StepVerifier.create(response)
                .expectNext(profesionalWithTurno)
                .verifyComplete();

    }

    @Test
    void eliminarTurnosProfesionalesAccionMasivaConCitasAsignadas(){

        List<EliminarTurnoProfesionalRequest> listaTurnoRequest = new ArrayList<>(){{add(
                EliminarTurnoProfesionalRequest.builder()
                        .fechaTurno(fechaTurno)
                        .idProfesional(idProfesional)
                        .build()
        );}};

       ResultadoActualizacionTurno resultadosActualizaciones = ResultadoActualizacionTurno.builder()
               .fechaTurno(fechaTurno)
               .mensaje(Mensajes.RESPUESTA_TURNO)
               .idProfesional(idProfesional)
               .build();

        List<CitaData> citasData = new ArrayList<>();
        citasData.add(citaData);

        Mockito.when(citaRepositoryMock.findAllByTurnoProfesional(fechaTurno,idProfesional))
                .thenReturn(Flux.fromIterable(citasData));


        Flux<ResultadoActualizacionTurno> response = secuenciasHorarioAdapter.eliminarTurnosProfesionalesAccionMasiva(
                listaTurnoRequest);

        StepVerifier.create(response)
                .expectNext(resultadosActualizaciones)
                .verifyComplete();
    }
    @Test
    void eliminarTurnosProfesionalesAccionMasivaSicitasAsignadas(){

        List<EliminarTurnoProfesionalRequest> listaTurnoRequest = new ArrayList<>(){{add(
                EliminarTurnoProfesionalRequest.builder()
                        .fechaTurno(fechaTurno)
                        .idProfesional(idProfesional)
                        .build()
        );}};

        List<CitaData> citasData = new ArrayList<>();

        Mockito.when(citaRepositoryMock.findAllByTurnoProfesional(fechaTurno,idProfesional))
                .thenReturn(Flux.fromIterable(citasData));

        Mockito.when(turnoProfesionalesRepositoryMock.eliminarByIdProfesionalFechaTurno(fechaTurno,idProfesional))
                .thenReturn(Mono.just(Boolean.TRUE));

        Flux<ResultadoActualizacionTurno> response = secuenciasHorarioAdapter.eliminarTurnosProfesionalesAccionMasiva(
                listaTurnoRequest);

        StepVerifier.create(response)
                .expectNextCount(0)
                .expectNext()
                .verifyComplete();
    }

    @Test
    void asignarTurnosProfesionalesAccionMasivaConCitas(){
        ResultadoActualizacionTurno resultadosActualizaciones = ResultadoActualizacionTurno.builder()
                .fechaTurno(fechaTurno)
                .mensaje(Mensajes.RESPUESTA_TURNO)
                .idProfesional(idProfesional)
                .build();

        List<TurnoProfesional> turnosProfesionales = new ArrayList<>();
        turnosProfesionales.add(turnoProfesional);

        List<CitaData> citasData = new ArrayList<>();
        citasData.add(citaData);

        Mockito.when(citaRepositoryMock.findAllByTurnoProfesional(fechaTurno,idProfesional))
                .thenReturn(Flux.fromIterable(citasData));

        Flux<ResultadoActualizacionTurno> response = secuenciasHorarioAdapter
                .asignarTurnosProfesionalesAccionMasiva(turnosProfesionales);

        StepVerifier.create(response)
                .expectNext(resultadosActualizaciones)
                .verifyComplete();
    }
    @Test
    void asignarTurnosProfesionalesAccionMasivasinCitas(){

        List<TurnoProfesional> turnosProfesionales = new ArrayList<>();
        turnosProfesionales.add(turnoProfesional);

        List<CitaData> citasData = new ArrayList<>();

        Mockito.when(citaRepositoryMock.findAllByTurnoProfesional(fechaTurno,idProfesional))
                .thenReturn(Flux.fromIterable(citasData));

        Mockito.when(turnoProfesionalesRepositoryMock.eliminarByIdProfesionalFechaTurno(fechaTurno,idProfesional))
                .thenReturn(Mono.just(Boolean.TRUE));

        Mockito.when(turnoProfesionalesRepositoryMock.save(turnoProfesionalesData))
                .thenReturn(Mono.just(turnoProfesionalesData));

        Flux<ResultadoActualizacionTurno> response = secuenciasHorarioAdapter
                .asignarTurnosProfesionalesAccionMasiva(turnosProfesionales);

        StepVerifier.create(response)
                .expectNextCount(0)
                .expectNext()
                .verifyComplete();
    }

    @Test
    void  consultarSecuencias(){

        HorarioTurnoData horarioTurnoData =   HorarioTurnoData.builder()
                .id(idHorarioTurno)
                .esHorarioBase(Boolean.TRUE)
                .build();

        HorarioTurno horarioTurno =   HorarioTurno.builder()
                .id(idHorarioTurno)
                .esHorarioBase(Boolean.TRUE)
                .build();

        List<ItemSecuenciaTurnoData> itemSecuenciaTurnoDataList = new ArrayList<>();
        itemSecuenciaTurnoDataList.add(itemSecuenciaTurnoData);

        Mockito.when(secuenciaTurnoRepositoryMock.findAll())
                .thenReturn(Flux.fromIterable(itemSecuenciaTurnoDataList));

        Mockito.when(horarioTurnoRepositoryMock.findAll())
                .thenReturn(Flux.fromIterable(new ArrayList<>(){{add(horarioTurnoData);}}));


        Flux<SecuenciaTurno> response = secuenciasHorarioAdapter.consultarSecuencias();


        StepVerifier.create(response)
                .expectNext(SecuenciaTurno.builder()
                        .nombre(itemSecuenciaTurnoData.getNombreSecuencia())
                        .descripcion(itemSecuenciaTurnoData.getDescripcion())
                        .itemsDiaTurno(new ArrayList<>(){{add(
                                ItemDiaTurno.builder()
                                        .nombreDia(itemSecuenciaTurnoData.getNombreDia())
                                        .numeroDia(itemSecuenciaTurnoData.getNumeroDia())
                                        .horariosTurno(new ArrayList<>(){{add(
                                                horarioTurno
                                        );}})
                                        .build()
                        );}})
                        .build())
                .expectComplete()
                .verify();
    }

    @Test
    void actualizarHorarioTurnoProfesionales(){
        List<TurnoProfesional> turnoProfesionalesDataList = new ArrayList<>();
        turnoProfesionalesDataList.add(turnoProfesional);

        Mockito.when(turnoProfesionalesRepositoryMock.eliminarByIdProfesionalFechaTurno(
        turnoProfesional.getFechaTurno(),turnoProfesional.getIdProfesional()))
                .thenReturn(Mono.just(Boolean.TRUE));

        Mockito.when(turnoProfesionalesRepositoryMock.saveAll(new ArrayList<>(){{add(turnoProfesionalesData);}}))
                .thenReturn(Flux.fromIterable(new ArrayList<>(){{add(turnoProfesionalesData);}}));

        Mono<Boolean> response = secuenciasHorarioAdapter
                .actualizarHorarioTurnoProfesionales(turnoProfesionalesDataList);

        StepVerifier.create(response)
                .expectNext(Boolean.TRUE)
                .verifyComplete();
    }

    @Test
    void configurarSecuenciaTurno(){
        HorarioTurno horarioTurno =   HorarioTurno.builder()
                .id(idHorarioTurno)
                .esHorarioBase(Boolean.TRUE)
                .build();

        SecuenciaTurno secuenciaTurno = SecuenciaTurno.builder()
                .nombre(itemSecuenciaTurnoData.getNombreSecuencia())
                .descripcion(itemSecuenciaTurnoData.getDescripcion())
                .itemsDiaTurno(new ArrayList<>(){{add(
                        ItemDiaTurno.builder()
                                .nombreDia(itemSecuenciaTurnoData.getNombreDia())
                                .numeroDia(itemSecuenciaTurnoData.getNumeroDia())
                                .horariosTurno(new ArrayList<>(){{add(horarioTurno);}})
                                .build());}})
                .build();

        Mockito.when(secuenciaTurnoRepositoryMock.deleteByNombreSecuencia(secuenciaTurno.getNombre()))
                            .thenReturn(Mono.empty());

        Mockito.when(secuenciaTurnoRepositoryMock.saveAll(new ArrayList<>(){{add(itemSecuenciaTurnoData);}}))
                .thenReturn(Flux.fromIterable(new ArrayList<>(){{add(itemSecuenciaTurnoData);}}));

        Mono<Boolean> response = secuenciasHorarioAdapter.configurarSecuenciaTurno(secuenciaTurno);

        StepVerifier.create(response)
                .expectNext(Boolean.TRUE)
                .verifyComplete();
    }


}

