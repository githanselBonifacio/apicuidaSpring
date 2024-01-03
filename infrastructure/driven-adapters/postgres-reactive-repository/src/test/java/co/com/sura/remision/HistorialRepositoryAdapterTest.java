package co.com.sura.remision;

import co.com.sura.postgres.agenda.adapter.ConverterAgenda;
import co.com.sura.postgres.agenda.adapter.ProcedimientosCitasAdapter;
import co.com.sura.postgres.agenda.data.CitaData;
import co.com.sura.postgres.agenda.repository.CitaRepository;
import co.com.sura.postgres.remision.adapter.ConverterRemision;
import co.com.sura.postgres.remision.adapter.HistorialRemisionAdapter;
import co.com.sura.postgres.remision.data.datospaciente.*;
import co.com.sura.postgres.remision.data.tratamientos.TratamientoData;
import co.com.sura.postgres.remision.repository.datospaciente.*;
import co.com.sura.postgres.remision.repository.tratamientos.TratamientoRepository;
import co.com.sura.remision.entity.historial.RegistroHistorialRemision;
import co.com.sura.remision.entity.procedimientos.Procedimientos;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
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
import java.util.stream.Collectors;

@ExtendWith(MockitoExtension.class)
 class HistorialRepositoryAdapterTest {

    private final String idRemision = "asf546";
    @Mock
    private  RemisionRepository remisionRepositoryMock;
    @Mock
    private  PacienteRepository pacienteRepositoryMock;
    @Mock
    private  UbicacionRepository ubicacionRepositoryMock;
    @Mock
    private  DatosAtencionPacienteRepository datosAtencionPacienteRepositoryMock;
    @Mock
    private  RemisionDiagnosticoRepository diagnosticoRepositoryMock;
    @Mock
    private  TratamientoRepository tratamientoRepositoryMock;
    @Mock
    private  ProcedimientosCitasAdapter procedimientosCitasAdapterMock;
    @Mock
    private  CitaRepository citaRepositoryMock;
    @Mock
    private  RegistroHistorialRepository registroHistorialRemisionRepositoryMock;

    private HistorialRemisionAdapter historialRemisionAdapter;

    @BeforeEach
    void setUp() {
        historialRemisionAdapter = new HistorialRemisionAdapter(
                remisionRepositoryMock,
                pacienteRepositoryMock,
                ubicacionRepositoryMock,
                datosAtencionPacienteRepositoryMock,
                diagnosticoRepositoryMock,
                tratamientoRepositoryMock,
                procedimientosCitasAdapterMock,
                citaRepositoryMock,
                registroHistorialRemisionRepositoryMock
        );
    }

    @Test
    void consultarHistoricoRemision(){
        List<RegistroHistorialRemisionData> registroHistorialRemisionData = new ArrayList<>();
        registroHistorialRemisionData.add(RegistroHistorialRemisionData.builder()
                .idRemision(idRemision)
                .build());

        Mockito.when(registroHistorialRemisionRepositoryMock.findAllByIdRemision(idRemision))
                .thenReturn(Flux.fromIterable(registroHistorialRemisionData));

        Flux<RegistroHistorialRemision> response = historialRemisionAdapter.consultarHistoricoRemision(idRemision);

        StepVerifier.create(response)
                .expectNextCount(1)
                .expectComplete()
                .verify();

    }

    @Test
    void consultarDatosRemision(){

        String mumeroIdentificacion = "154565415";
        RemisionData remisionData = RemisionData.builder()
                .idRemision(idRemision)
                .numeroIdentificacionPaciente(mumeroIdentificacion)
                .fechaAdmision(LocalDate.now())
                .build();

        PacienteData paciente = PacienteData.builder()
                .numeroIdentificacion(mumeroIdentificacion)
                .build();

        List<RemisionDiagnosticoData> remisionesDiagnosticos = new ArrayList<>();
        remisionesDiagnosticos.add(RemisionDiagnosticoData.builder().build());

        UbicacionData ubicacionData = UbicacionData.builder().build();

        DatosAtencionPacienteData datosAtencionPacienteData = DatosAtencionPacienteData.builder().build();

        //citas
        List<CitaData> citasData = new ArrayList<>();
        List<TratamientoData> tratamientosData = new ArrayList<>();
        TratamientoData tratamientoData = TratamientoData.builder()
                .noPBS(Boolean.FALSE)
                .build();
        tratamientosData.add(tratamientoData);

        Procedimientos procedimientos = Procedimientos.builder().build();

        CitaData citaData = CitaData.builder()
                .idCita(idRemision+"-1")
                .idRemision(idRemision)
                .duracion(600)
                .idEstado(1)
                .holgura(600)
                .fechaInicio(LocalDateTime.of(LocalDate.now(), LocalTime.of(8,10)))
                .fechaProgramada(LocalDateTime.of(LocalDate.now(), LocalTime.of(8,10)))
                .especialidad("especialidad")
                .idRegional("427")
                .idProfesional(null)
                .idConductor(null)
                .latitud(0.0)
                .longitud(0.0)

                .build();

        citasData.add(citaData);
        //response
        RegistroHistorialRemision registroHistorialRemision = RegistroHistorialRemision
                .builder()
                .idRemision(idRemision)
                .fechaAdmision(LocalDate.now())
                .paciente(ConverterRemision.convertToPaciente(paciente))
                .datosAtencion(datosAtencionPacienteData)
                .diagnosticos(remisionesDiagnosticos)
                .ubicacionPaciente(ubicacionData)
                .citas(citasData.stream()
                        .map(ConverterAgenda::convertToCitaHistorial)
                        .collect(Collectors.toList()))
                .build();

        Mockito.when(remisionRepositoryMock.findById(idRemision))
                .thenReturn(Mono.just(remisionData));

        Mockito.when(pacienteRepositoryMock.findPacienteByNumeroIdRemision(idRemision))
                .thenReturn(Mono.just(paciente));

        Mockito.when(ubicacionRepositoryMock.findByIdRemision(mumeroIdentificacion))
                        .thenReturn(Mono.just(ubicacionData));

        Mockito.when(datosAtencionPacienteRepositoryMock.findByIdRemision(idRemision))
                .thenReturn(Mono.just(datosAtencionPacienteData));

        Mockito.when(diagnosticoRepositoryMock.findAllByIdRemision(idRemision))
                .thenReturn(Flux.fromIterable(remisionesDiagnosticos));

        Mockito.when(citaRepositoryMock.findAllByIdRemision(idRemision))
                .thenReturn(Flux.fromIterable(citasData));

       Mockito.when(tratamientoRepositoryMock.findByIdCita(idRemision+"-1"))
                .thenReturn(Flux.fromIterable(tratamientosData));

        Mockito.when(procedimientosCitasAdapterMock.consultarProcedimientosByIdCita(idRemision+"-1"))
                .thenReturn(Mono.just(procedimientos));

        Mockito.when(tratamientoRepositoryMock.findByIdCita(idRemision+"-1"))
                .thenReturn(Flux.fromIterable(tratamientosData));

        Mono<RegistroHistorialRemision> response = historialRemisionAdapter.consultarDatosRemision(idRemision);



       StepVerifier.create(response)
              .expectNext(registroHistorialRemision)
               .expectComplete()
               .verify();

    }

    @Test
    void insertRegistro(){
        RegistroHistorialRemisionData registroHistorialRemisionData  = RegistroHistorialRemisionData
                .builder()
                .id("id")
                .build();


        Mockito.when(registroHistorialRemisionRepositoryMock.save(registroHistorialRemisionData))
                .thenReturn(Mono.just(registroHistorialRemisionData));

        Mono<Void> response = historialRemisionAdapter
                .insertRegistro(registroHistorialRemisionData);



        StepVerifier.create(response)
                .expectNext()
                .expectComplete()
                .verify();
    }

}
