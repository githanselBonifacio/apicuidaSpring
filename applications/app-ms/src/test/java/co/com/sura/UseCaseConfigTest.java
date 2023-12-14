package co.com.sura;

import co.com.sura.agenda.AgendaUseCase;
import co.com.sura.agenda.gateway.AgendaRepository;
import co.com.sura.agenda.gateway.AgendamientoAutomaticoRepository;
import co.com.sura.agenda.gateway.GestionEstadosCitasRepository;
import co.com.sura.config.UseCaseConfig;
import co.com.sura.farmacia.FarmaciaUseCase;
import co.com.sura.farmacia.gateway.FarmaciaRepository;
import co.com.sura.maestro.CrudMaestroUseCase;
import co.com.sura.maestros.gateway.MaestroRepository;
import co.com.sura.moviles.MovilesUseCase;
import co.com.sura.personal.PersonalUseCase;
import co.com.sura.personal.gateway.PersonalCrudRepository;
import co.com.sura.personal.gateway.SecuenciasHorarioRepository;
import co.com.sura.remision.RemisionUseCase;
import co.com.sura.remision.gateway.HistorialRemisionRepository;
import co.com.sura.remision.gateway.RemisionCrudRepository;
import co.com.sura.reportes.ReportesUseCase;
import co.com.sura.reportes.gateway.ReportesRepository;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.junit.jupiter.SpringExtension;

@ExtendWith(SpringExtension.class)
@SpringBootTest
class UseCaseConfigTest {
    @Mock
    private MaestroRepository maestroRepositoryMock;
    @Mock
    private AgendaRepository agendaRepositoryMock;
    @Mock
    private PersonalCrudRepository personalCrudRepositoryMock;
    @Mock
    private GestionEstadosCitasRepository gestionEstadosCitasRepositoryMock;
    @Mock
    private AgendamientoAutomaticoRepository agendamientoAutomaticoRepositoryMock;
    @Mock
    private RemisionCrudRepository remisionCrudRepositoryMock;
    @Mock
    private HistorialRemisionRepository historialRemisionRepositoryMock;
    @Mock
    private FarmaciaRepository farmaciaRepositoryMock;

    @Mock
    private ReportesRepository movilRepositoryMock;

    @Mock
    ReportesRepository reportesRepositoryMock;

    @Mock
    PersonalCrudRepository personalRepositoryMock;

    @Mock
    SecuenciasHorarioRepository secuenciasHorarioRepositorMock;
    @InjectMocks
    private UseCaseConfig useCaseConfig;

    @Test
     void crearCrudMaestroUseCase() {
        CrudMaestroUseCase crudMaestroUseCase = useCaseConfig.crudMaestroUseCase(maestroRepositoryMock);
        Assertions.assertNotNull(crudMaestroUseCase);
    }

    @Test
    void crearAgendaUseCase() {
        AgendaUseCase agendaUseCase = useCaseConfig.agendaUseCase(
                agendaRepositoryMock,
                personalCrudRepositoryMock,
                gestionEstadosCitasRepositoryMock,
                agendamientoAutomaticoRepositoryMock
        );
        Assertions.assertNotNull(agendaUseCase);
    }

    @Test
    void crearRemisionUseCase() {
        RemisionUseCase remisionUseCase = useCaseConfig.remisionUseCase(
                remisionCrudRepositoryMock,
                historialRemisionRepositoryMock
        );
        Assertions.assertNotNull(remisionUseCase);
    }

    @Test
    void crearFarmaciaUseCase() {
        FarmaciaUseCase farmaciaUseCase = useCaseConfig.farmaciaUseCase(
                farmaciaRepositoryMock
        );
        Assertions.assertNotNull(farmaciaUseCase);
    }

    @Test
    void crearMovilesCase() {
        MovilesUseCase movilesUseCase = useCaseConfig.movilesUseCase(
                movilRepositoryMock
        );
        Assertions.assertNotNull(movilesUseCase);
    }

    @Test
    void crearReporteCase() {
        ReportesUseCase reportesUseCase = useCaseConfig.reportesUseCase(
                reportesRepositoryMock
        );
        Assertions.assertNotNull(reportesUseCase);
    }

    @Test
    void crearPersonalCase() {
        PersonalUseCase personalUseCase = useCaseConfig.personalUseCase(
                personalCrudRepositoryMock,
                secuenciasHorarioRepositorMock
        );
        Assertions.assertNotNull(personalUseCase);
    }
}