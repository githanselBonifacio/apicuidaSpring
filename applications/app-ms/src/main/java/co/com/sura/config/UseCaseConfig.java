package co.com.sura.config;

import co.com.sura.agenda.AgendaUseCase;
import co.com.sura.entity.agenda.AgendaRepository;
import co.com.sura.entity.maestro.MaestroRepository;
import co.com.sura.entity.moviles.MovilRepository;
import co.com.sura.entity.admin.RemisionCrudRepository;
import co.com.sura.entity.reportes.ReportesRepository;
import co.com.sura.maestro.CrudMaestroUseCase;
import co.com.sura.moviles.MovilesUseCase;
import co.com.sura.admin.AdminUseCase;
import co.com.sura.moviles.reportes.ReportesUseCase;
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
    public AgendaUseCase agendaUseCase (AgendaRepository agendaRepository){
        return new AgendaUseCase(agendaRepository);
    }

    @Bean
    public AdminUseCase remisionUseCase (RemisionCrudRepository remisionCrudRepository){
        return new AdminUseCase(remisionCrudRepository);
    }

    @Bean
    public MovilesUseCase movilesUseCase (MovilRepository movilRepository){
        return new MovilesUseCase(movilRepository);
    }

    @Bean
    public ReportesUseCase reportesUseCase (ReportesRepository reportesRepository){
        return new ReportesUseCase(reportesRepository);
    }

}
