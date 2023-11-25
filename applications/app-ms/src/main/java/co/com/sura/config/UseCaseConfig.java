package co.com.sura.config;

import co.com.sura.agenda.AgendaUseCase;
import co.com.sura.entity.agenda.AgendaRepository;
import co.com.sura.entity.agenda.AgendamientoAutomaticoRepository;
import co.com.sura.entity.agenda.GestionEstadosCitasRepository;
import co.com.sura.entity.maestro.MaestroRepository;
import co.com.sura.entity.moviles.MovilRepository;
import co.com.sura.entity.personal.SecuenciasHorarioRepository;
import co.com.sura.entity.remision.RemisionCrudRepository;
import co.com.sura.entity.personal.PersonalCrudRepository;
import co.com.sura.entity.reportes.ReportesRepository;
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
    public RemisionUseCase remisionUseCase (RemisionCrudRepository remisionCrudRepository){
        return new RemisionUseCase(remisionCrudRepository);
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
    public PersonalUseCase personalesUseCase (PersonalCrudRepository personalRepository,
                                              SecuenciasHorarioRepository secuenciasHorarioRepository){
        return new PersonalUseCase(personalRepository, secuenciasHorarioRepository);
    }

}
