package co.com.sura.config;

import co.com.sura.agenda.AgendaUseCase;
import co.com.sura.agenda.gateway.AgendaRepository;
import co.com.sura.agenda.gateway.AgendamientoAutomaticoRepository;
import co.com.sura.agenda.gateway.GestionEstadosCitasRepository;
import co.com.sura.farmacia.gateway.FarmaciaRepository;
import co.com.sura.maestros.gateway.MaestroRepository;
import co.com.sura.moviles.gateway.MovilRepository;
import co.com.sura.personal.gateway.SecuenciasHorarioRepository;
import co.com.sura.remision.gateway.HistorialRemisionRepository;
import co.com.sura.remision.gateway.RemisionCrudRepository;
import co.com.sura.personal.gateway.PersonalCrudRepository;
import co.com.sura.reportes.gateway.ReportesRepository;
import co.com.sura.farmacia.FarmaciaUseCase;
import co.com.sura.maestro.CrudMaestroUseCase;
import co.com.sura.moviles.MovilesUseCase;
import co.com.sura.remision.RemisionUseCase;
import co.com.sura.personal.PersonalUseCase;
import co.com.sura.reportes.ReportesUseCase;
import lombok.RequiredArgsConstructor;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;



@RequiredArgsConstructor
@Configuration
public class UseCaseConfig {


    @Bean
    public CrudMaestroUseCase crudMaestroUseCase (MaestroRepository maestroRepository){
        return new CrudMaestroUseCase(maestroRepository);
    }

    @Bean
    public AgendaUseCase agendaUseCase (AgendaRepository agendaRepository,
                                        PersonalCrudRepository personalCrudRepository,
                                        GestionEstadosCitasRepository gestionEstadosCitasRepository,
                                        AgendamientoAutomaticoRepository agendamientoAutomaticoRepository){

        return new AgendaUseCase(agendaRepository, personalCrudRepository,
                                 gestionEstadosCitasRepository, agendamientoAutomaticoRepository);
    }

    @Bean
    public RemisionUseCase remisionUseCase (RemisionCrudRepository remisionCrudRepository,
                                            HistorialRemisionRepository historialRemisionRepository){
        return new RemisionUseCase(remisionCrudRepository, historialRemisionRepository);
    }
    @Bean
    public FarmaciaUseCase farmaciaUseCase (FarmaciaRepository farmaciaRepository){
        return  new FarmaciaUseCase(farmaciaRepository);
    }
    @Bean
    public MovilesUseCase movilesUseCase (MovilRepository movilRepository){
        return new MovilesUseCase(movilRepository);
    }

    @Bean
    public ReportesUseCase reportesUseCase (ReportesRepository reportesRepository){
        return new ReportesUseCase(reportesRepository);
    }

    @Bean
    public PersonalUseCase personalUseCase(PersonalCrudRepository personalRepository,
                                           SecuenciasHorarioRepository secuenciasHorarioRepository){
        return new PersonalUseCase(personalRepository, secuenciasHorarioRepository);
    }

}
