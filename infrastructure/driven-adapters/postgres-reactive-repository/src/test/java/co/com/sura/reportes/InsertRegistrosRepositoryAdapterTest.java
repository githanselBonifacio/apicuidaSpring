package co.com.sura.reportes;

import co.com.sura.genericos.EstadosCita;
import co.com.sura.postgres.agenda.data.CitaData;
import co.com.sura.postgres.agenda.repository.CitaRepository;
import co.com.sura.postgres.maestros.data.RegionalData;
import co.com.sura.postgres.maestros.repository.RegionalesRepository;
import co.com.sura.postgres.moviles.data.DesplazamientoData;
import co.com.sura.postgres.moviles.data.DesplazamientoRepository;
import co.com.sura.postgres.personal.repository.TurnoProfesionalesRepository;
import co.com.sura.postgres.remision.repository.datospaciente.RegistroHistorialRepository;
import co.com.sura.postgres.remision.repository.datospaciente.RemisionRepository;
import co.com.sura.postgres.reportes.adapter.RegistroReportesAdapter;
import co.com.sura.postgres.reportes.data.ReporteTurnoData;
import co.com.sura.postgres.reportes.repository.ReporteTurnoRepository;
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
import java.time.ZoneId;
import java.util.Collections;

@ExtendWith(MockitoExtension.class)
class InsertRegistrosRepositoryAdapterTest {

    @Mock
    private  RegionalesRepository regionalesRepositoryMock;
    @Mock
    private  RemisionRepository remisionRepositoryMock;
    @Mock
    private  CitaRepository citaRepositoryMock;
    @Mock
    private  DesplazamientoRepository desplazamientoRepositoryMock;
    @Mock
    private  TurnoProfesionalesRepository turnoProfesionalesRepositoryMock;
    @Mock
    private  RegistroHistorialRepository registroHistorialRepositoryMock;
    @Mock
    private  ReporteTurnoRepository reportesTurnoRepositoryMock;

    @InjectMocks
    private RegistroReportesAdapter reportesAdapter;

    @Test
    void actualizarReporteTurno(){

        //data
        var zone = ZoneId.of("America/Bogota");
        LocalDate fechaturno = LocalDate.now(zone).minusDays(1);
        String idRegional = "427";
        Integer idHorarioTurno = 1;
        Integer numeroRemisiones = 1;
        Integer numeroNovedades= 1;
        Integer numeroProfesionalesTurno = 5;
        Integer duracion = 600;

        RegionalData regionalData = RegionalData.builder()
                .id(idRegional)
                .build();

        CitaData citaData = CitaData.builder()
                .fechaInicio(fechaturno.atStartOfDay())
                .fechaProgramada(fechaturno.atStartOfDay())
                .idRegional(idRegional)
                .idHorarioTurno(idHorarioTurno)
                .duracion(duracion)
                .idEstado(EstadosCita.FINALIZADA.getEstado())
                .build();

        DesplazamientoData desplazamientoData = DesplazamientoData.builder()
                .fechaProgramada(fechaturno.atStartOfDay())
                .idRegional(idRegional)
                .idHorarioTurno(idHorarioTurno)
                .duracion(duracion)
                .build();

        ReporteTurnoData reporteTurnoData = ReporteTurnoData.builder()
                .fechaTurno(fechaturno)
                .idRegional(idRegional)
                .build();
        //mocks
        Mockito.when(reportesTurnoRepositoryMock.deleteByFechaturno(fechaturno))
                .thenReturn(Mono.empty());

        Mockito.when(reportesTurnoRepositoryMock.save(reporteTurnoData))
                .thenReturn(Mono.just(reporteTurnoData));

        Mockito.when(regionalesRepositoryMock.findAll())
                .thenReturn(Flux.fromIterable(Collections.singletonList(regionalData)));


        Mockito.when(citaRepositoryMock.findAllByFechaTurnoRegional(fechaturno,idRegional))
                .thenReturn(Flux.fromIterable(Collections.singletonList(citaData)));

        Mockito.when(remisionRepositoryMock.countAllByFechaAdmisionIdRegional(fechaturno,idRegional))
                .thenReturn(Mono.just(numeroRemisiones));

        Mockito.when(registroHistorialRepositoryMock.countByFechaNovedadRegional(fechaturno,idRegional))
                .thenReturn(Mono.just(numeroNovedades));

        Mockito.when(turnoProfesionalesRepositoryMock.countByFechaTurno(fechaturno,idRegional))
                .thenReturn(Mono.just(numeroProfesionalesTurno));

        Mockito.when(desplazamientoRepositoryMock.findByFechaProgramadaRegional(fechaturno,idRegional))
                .thenReturn(Flux.fromIterable(Collections.singletonList(desplazamientoData)));

        Mono<Boolean> response = reportesAdapter.actualizarReporteTurno();

        StepVerifier.create(response)
                .expectNext(Boolean.TRUE)
                .verifyComplete();
    }
}
